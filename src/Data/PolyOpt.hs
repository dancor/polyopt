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
    print args
    print $ verbose opts
    print $ color opts
    print $ showDecimal opts

This is what gets generated:
  data Opts = Opts {
    version :: Bool,
    color :: Maybe String,
    showDecimal :: Maybe (Maybe String)}

  defOpts :: Opts
  defOpts = Opts {
    version = False,
    color = Nothing,
    showDecimal = Nothing}

  getOpts :: String -> IO (Opts, [String])
  getOpts header = do
    let
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
    args <- getArgs
    return $ case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) defOpts o, n)
      (_, _, e) -> error $ concat e ++ usageInfo header options
-}

import Control.Applicative
import Control.Monad
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.ConfigFile
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

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c = PolyOpt (namesToRecName n) n c NoArgInfo

reqArg :: [String] -> [Char] -> String -> String -> PolyOpt
reqArg n c = PolyOpt (namesToRecName n) n c . ReqArgInfo

optArg :: [String] -> [Char] -> String -> String -> PolyOpt
optArg n c = PolyOpt (namesToRecName n) n c . OptArgInfo

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

--
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
        configOptNames = either (const []) id $ options config "DEFAULT"
        defOpts = $(return $ RecConE optsN defRecs)
        optOptions = $(ListE <$> mapM optToOption opts)
        --configOpts = $(foldM defOpts configOptNames
        --configOpts = $(
        configOpts = defOpts
      {-
      case configOpts of
        [] ->
        overlap -> error
      -}
      --print configOpts
      args <- getArgs
      return $ case getOpt Permute optOptions args of
        (o, n, []) -> (foldl (flip id) configOpts o, n)
        (_, _, e) -> error $ concat e ++ usageInfo header optOptions |]

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
