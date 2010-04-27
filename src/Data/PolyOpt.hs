{-# LANGUAGE TemplateHaskell #-}

module Data.PolyOpt (PolyOpt, polyOpt, noArg, reqArg, optArg,
  noArgGen, reqArgGen, optArgGen) where

{-
PolyOpt will allow a no-repetition specification of program options and then
automatically make those options work simultaneously as command line options
and as Data.ConfigFile options.

Usage example:

Opt.hs:
  {-# LANGUAGE TemplateHaskell #-}

  module Opt where

  import Data.PolyOpt

  $(polyOpt [
    noArg ["version"] "v"
      "Print version",
    reqArg ["color","colour"] "c"
      "NAME"
      "Foreground color",
    optArgGen ["show-decimal"] ""
      "N" [t|Int|] [|0|] [|maybe 8 read|]
      "Show full decimal precision, or to N digits"])

Main.hs:
  import System.Directory
  import System.FilePath
  import qualified Opt

  main :: IO ()
  main = do
    homeDir <- getHomeDirectory
    (opts, args) <- Opt.getOpts (homeDir </> ".lol" </> "config") "usage"
    print $ Opt.verbose opts
    print $ Opt.color opts
    print $ Opt.showDecimal opts
    print args

~/.lol/config (could contain something like this, or not exist):
  version = True
  color = red
  show-decimal = 4

This is what gets generated (but ConfigFile part not shown here yet..):
  data Opts = Opts {
    version :: Bool,
    color :: Maybe String,
    showDecimal :: Int}

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
import System.Console.GetOpt hiding (usageInfo)
import System.Directory
import System.Environment
import qualified System.Console.GetOpt as GetOpt

data ArgInfo =
  NoArgInfo {
    argType :: Q Type,
    argDef :: Q Exp,
    -- :: a
    argGrab :: Q Exp} |
  ReqArgInfo {
    argGloss :: String,
    argType :: Q Type,
    argDef :: Q Exp,
    -- :: String -> a
    argGrab :: Q Exp} |
  OptArgInfo {
    argGloss :: String,
    argType :: Q Type,
    argDef :: Q Exp,
    -- :: Maybe String -> a
    argGrab :: Q Exp}

data PolyOpt = PolyOpt {
  recName :: String,
  names :: [String],
  chars :: [Char],
  argInfo :: ArgInfo,
  help :: String}

noArgGen :: [String] -> [Char] -> Q Type -> Q Exp -> Q Exp -> String -> PolyOpt
noArgGen n c t d = PolyOpt (genRecName n) n c . NoArgInfo t d

reqArgGen, optArgGen ::
  [String] -> [Char] -> String -> Q Type -> Q Exp -> Q Exp -> String -> PolyOpt
reqArgGen n c g t d = PolyOpt (genRecName n) n c . ReqArgInfo g t d
optArgGen n c g t d = PolyOpt (genRecName n) n c . OptArgInfo g t d

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c = noArgGen n c [t| Bool |] [| False |] [| True |]

reqArg, optArg :: [String] -> [Char] -> String -> String -> PolyOpt
reqArg n c g = reqArgGen n c g [t| Maybe String |] [| Nothing |] [| Just |]
optArg n c g =
  optArgGen n c g [t| Maybe (Maybe String) |] [| Nothing |] [| Just . Just |]

genRecName :: [String] -> String
genRecName = dashToCamel . head

dashToCamel :: String -> String
dashToCamel [] = []
dashToCamel ('-':'-':_) =
  error "polyOpt: double dash in option name not allowed"
dashToCamel ('-':x:xs) = toUpper x : dashToCamel xs
dashToCamel ['-'] = error "polyOpt: trailing dash in option name not allowed"
dashToCamel (x:xs) = x : dashToCamel xs

optBoxInfo :: PolyOpt -> Q (Name, Type)
optBoxInfo opt = (,)
  (mkName $ recName opt) <$> (argType $ argInfo opt)

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  optInfos <- mapM optBoxInfo opts
  let
    optsN = mkName "Opts"
    -- is IsStrict better?  i have no idea, probably doesn't matter
    optRecords = map (\ (n, t) -> (n, NotStrict, t)) optInfos
    optNames = map fst optInfos
  defRecs <- zip optNames <$> mapM (argDef . argInfo) opts
  (DataD [] optsN [] [RecC optsN optRecords] [] :) <$> [d|
    -- this would be compiled twice without a top-lev decl right?  too lame?
    __optOptions = $(ListE <$> mapM optToOption opts)

    usageInfo :: String -> String
    usageInfo header = GetOpt.usageInfo header __optOptions

    --getOpts :: FilePath -> String -> IO ($(return $ ConT optsN), [String])
    getOpts configFile header = do
      config <- doesFileExist configFile >>= \ t -> if t
        then either (const emptyCP) id <$> readfile emptyCP configFile
        else return emptyCP
      let
        configOptNames = either (const []) id $ options config defSection
        defOpts = $(return $ RecConE optsN defRecs)
        configOpts = foldl $(processConfigOpt 'config opts) defOpts
          configOptNames
      args <- getArgs
      return $ case getOpt Permute __optOptions args of
        (o, n, []) -> (foldl (flip id) configOpts o, n)
        (_, _, e) -> error $ concat e ++ usageInfo header |]

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
      (get $(return $ VarE configN) defSection $(return $ VarE optNameN)
      :: Either CPError String) |]) $
    names polyOpt
    where
    procFunc = case argInfo polyOpt of
      NoArgInfo _ _ _ -> [| const $grab |]
      ReqArgInfo _ _ _ _ -> grab
      OptArgInfo _ _ _ _-> [| $grab . Just |]
    grab = argGrab $ argInfo polyOpt

optToOption :: PolyOpt -> Q Exp
optToOption opt =
  [| Option $(lift $ chars opt) $(lift $ names opt) $f $(lift $ help opt) |]
  where
  name = mkName $ recName opt
  f = case argInfo opt of
    NoArgInfo _ _ _ -> [| NoArg (\ o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> grab)) |]
    ReqArgInfo g _ _ _ -> [| ReqArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| $grab a |])) g |]
    OptArgInfo g _ _ _ -> [| OptArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| $grab a |])) g |]
  grab = argGrab $ argInfo opt
