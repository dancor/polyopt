{-# LANGUAGE TemplateHaskell #-}

module Data.PolyOpt (PolyOpt, polyOpt, noArg, reqArg, optArg) where

{-
PolyOpt will allow a no-repetition specification of program options and then
automatically make those options work simultaneously as command line options
and as Data.ConfigFile options.

Usage example (currently just have the TH Opts datatype constructor):

OptDesc.hs:
  module OptDesc where

  import Data.PolyOpt

  optDesc :: [PolyOpt]
  optDesc = [
    noArg ["version"] "v"
      "Print version",
    reqArg ["color","colour"] "c"
      "NAME"
      "Foreground color",
    optArg ["show-decimal"] ""
      "N"
      "Show full decimal precision, or to N digits"]

Main.hs:
  {-# LANGUAGE TemplateHaskell #-}

  import Data.PolyOpt
  -- GHC TH seems to require optDesc in a separate module:
  import OptDesc

  $(polyOpt optDesc)

  main :: IO ()
  main = do
    (opts, args) <- getOpts "usage"
    print $ verbose opts
    print $ color opts
    print $ showDecimal opts
    print args

This is what gets generated (but ConfigFile part not shown here yet..):
  data Opts = Opts {
    version :: Bool,
    color :: Maybe String,
    showDecimal :: Maybe (Maybe String)}

  getOpts :: FilePath -> String -> IO (Opts, [String])
  getOpts configFile header = do
    let
      defOpts :: Opts
      defOpts = Opts {
        version = False,
        color = Nothing,
        showDecimal = Nothing}
      options :: [OptDescr (Opts -> Opts)]
      options = [
        Option "v" ["version"]
          (NoArg (\ o -> o {version = True}))
          "Print version",
        Option "c" ["color", "colour"]
          (ReqArg (\ a o -> o {color = Just a}) "NAME")
          "Foreground color",
        Option "" ["show-decimal"]
          (OptArg (\ a o -> o {showDecimal = Just a}) "N")
          "Show full decimal precision, or to N digits"]
    configOpts <- todoShowHowConfigIsProcessed configFile defOpts
    args <- getArgs
    return $ case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) configOpts o, n)
      (_, _, e) -> error $ concat e ++ usageInfo header options
-}

import Control.Applicative
import Control.Monad
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.ConfigFile
import Data.Either.Utils
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Console.GetOpt
import System.Directory
import System.Environment

data ArgInfo =
  NoArgInfo |
  ReqArgInfo {
    argGloss :: String} |
  OptArgInfo {
    argGloss :: String}

data PolyOpt = PolyOpt {
  recName :: String,
  names :: [String],
  chars :: [Char],
  argInfo :: ArgInfo,
  help :: String}

noArg :: [String] -> String -> [Char] -> PolyOpt
noArg n c = PolyOpt (namesToRecName n) n c NoArgInfo

reqArg :: [String] -> String -> [Char] -> String -> PolyOpt
reqArg n c = PolyOpt (namesToRecName n) n c . ReqArgInfo

optArg :: [String] -> String -> [Char] -> String -> PolyOpt
optArg n c = PolyOpt (namesToRecName n) n c . OptArgInfo

namesToRecName :: [String] -> String
namesToRecName = dashToCamel . head

dashToCamel :: String -> String
dashToCamel [] = []
dashToCamel ('-':'-':_) =
  error "polyOpt: double dash in option name not allowed"
dashToCamel ('-':x:xs) = toUpper x : dashToCamel xs
dashToCamel ['-'] = error "polyOpt: trailing dash in option name not allowed"
dashToCamel (x:xs) = x : dashToCamel xs

optBoxInfo :: PolyOpt -> Q (Name, Type)
optBoxInfo opt = (,)
  (mkName $ recName opt) <$>
  case argInfo opt of
    NoArgInfo -> [t| Bool |]
    ReqArgInfo _ -> [t| Maybe String |]
    OptArgInfo _ -> [t| Maybe (Maybe String) |]

defGen :: PolyOpt -> Exp
defGen opt = ConE $ case argInfo opt of
  NoArgInfo -> 'False
  _ -> 'Nothing

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  optInfos <- mapM optBoxInfo opts
  let
    defRecs = zip optNames $ map defGen opts
    optsN = mkName "Opts"
    -- is IsStrict better?  i have no idea, probably doesn't matter
    optRecords = map (\ (n, t) -> (n, NotStrict, t)) optInfos
    optNames = map fst optInfos
  (DataD [] optsN [] [RecC optsN optRecords] [] :) <$> [d|
    --getOpts :: FilePath -> String -> IO ($optsN, [String])
    getOpts configFile header = do
      config <- doesFileExist configFile >>= \ t -> if t
        then either (const emptyCP) id <$> readfile emptyCP configFile
        else return emptyCP
      let
        configOptNames = either (const []) id $ options config defSection
        defOpts = $(return $ RecConE optsN defRecs)
        optOptions = $(ListE <$> mapM optToOption opts)
        configOpts = foldl $(processConfigOpt 'config opts) defOpts
          configOptNames
      args <- getArgs
      return $ case getOpt Permute optOptions args of
        (o, n, []) -> (foldl (flip id) configOpts o, n)
        (_, _, e) -> error $ concat e ++ usageInfo header optOptions |]

defSection :: String
defSection = "DEFAULT"

processConfigOpt :: Name -> [PolyOpt] -> ExpQ
processConfigOpt configN optDesc = [| \ opts optName ->
  $(CaseE (VarE 'optName) . concat <$>
  mapM (optMatch 'optName 'opts) optDesc) |]
  where
  -- unrolling over names is lame?
  optMatch :: Name -> Name -> PolyOpt -> Q [Match]
  optMatch optNameN optsN polyOpt =
    mapM (\ n -> (\ v -> Match (LitP $ StringL n)
    (NormalB $ RecUpdE (VarE optsN) [(mkName $ recName polyOpt, v)]) []) <$>
    [| $procFunc . forceEither $
      get $(return $ VarE configN) defSection $(return $ VarE optNameN) |]) $
    names polyOpt
    where
    procFunc = case argInfo polyOpt of
      NoArgInfo -> [| id |]
      ReqArgInfo _ -> [| Just |]
      OptArgInfo _ -> [| Just . Just |]

optToOption :: PolyOpt -> Q Exp
optToOption opt =
  [| Option $(lift $ chars opt) $(lift $ names opt) $f $(lift $ help opt) |]
  where
  name = mkName $ recName opt
  f = case argInfo opt of
    NoArgInfo -> [| NoArg (\ o ->
      $(return $ RecUpdE (VarE 'o) [(name, ConE 'True)])) |]
    ReqArgInfo g -> [| ReqArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| Just a |])) g |]
    OptArgInfo g -> [| OptArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| Just a |])) g |]
