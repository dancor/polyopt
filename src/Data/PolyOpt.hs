{-# LANGUAGE ExistentialQuantification #-}
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
    optArg ["show-decimal"] ""
      "N"
      "Show full decimal precision, or to N digits"])

Main.hs:
  import qualified Opt

  main :: IO ()
  main = do
    (opts, args) <- Opt.getOpts "lol.config" "usage"
    print $ Opt.verbose opts
    print $ Opt.color opts
    print $ Opt.showDecimal opts
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
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Console.GetOpt
import System.Directory
import System.Environment

-- we could also make a separate argType :: Q Exp instead of
-- inferring/annotating from argDef.
-- this would remove Lift a constraint (so e.g. functions could be result) and
-- kill tOf hack.. should consider.
data ArgInfo a =
  NoArgInfo {
    argDef :: a,
    -- argGrab :: a,
    argGrab :: Q Exp} |
  ReqArgInfo {
    argGloss :: String,
    argDef :: a,
    -- argGrab :: String -> a,
    argGrab :: Q Exp} |
  OptArgInfo {
    argGloss :: String,
    argDef :: a,
    -- argGrab :: Maybe String -> a,
    argGrab :: Q Exp}

data PolyOptA a = PolyOptA {
  recName :: String,
  names :: [String],
  chars :: [Char],
  argInfo :: ArgInfo a,
  help :: String}

data PolyOpt = forall a. (Lift a, Typeable a) => PolyOpt (PolyOptA a)

noArgGen :: (Lift a, Typeable a) =>
  [String] -> [Char] -> a -> Q Exp -> String -> PolyOpt
noArgGen n c d r = PolyOpt . PolyOptA (genRecName n) n c (NoArgInfo d r)

reqArgGen :: (Lift a, Typeable a) => [String] -> [Char] -> String -> a ->
  Q Exp -> String -> PolyOpt
reqArgGen n c g d r = PolyOpt . PolyOptA (genRecName n) n c (ReqArgInfo g d r)

optArgGen :: (Lift a, Typeable a) => [String] -> [Char] -> String -> a ->
  Q Exp -> String -> PolyOpt
optArgGen n c g d r = PolyOpt . PolyOptA (genRecName n) n c (OptArgInfo g d r)

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c = noArgGen n c False [| True |]

reqArg :: [String] -> [Char] -> String -> String -> PolyOpt
reqArg n c g = reqArgGen n c g (Nothing :: Maybe String) [| Just |]

optArg :: [String] -> [Char] -> String -> String -> PolyOpt
optArg n c g = optArgGen n c g (Nothing :: Maybe (Maybe String))
  [| Just . Just |]

genRecName :: [String] -> String
genRecName = dashToCamel . head

-- there might be a TH way to do this that doesn't need Typeable at all..
-- unsure
tOf :: (Typeable a) => a -> Type
tOf x = either error id . parseType $ showsTypeRep (typeOf x) ""

dashToCamel :: String -> String
dashToCamel [] = []
dashToCamel ('-':'-':_) =
  error "polyOpt: double dash in option name not allowed"
dashToCamel ('-':x:xs) = toUpper x : dashToCamel xs
dashToCamel ['-'] = error "polyOpt: trailing dash in option name not allowed"
dashToCamel (x:xs) = x : dashToCamel xs

optBoxInfo :: PolyOpt -> Q (Name, Type)
optBoxInfo (PolyOpt opt) = (,)
  (mkName $ recName opt) <$> return (tOf . argDef $ argInfo opt)

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  optInfos <- mapM optBoxInfo opts
  let
    optsN = mkName "Opts"
    -- is IsStrict better?  i have no idea, probably doesn't matter
    optRecords = map (\ (n, t) -> (n, NotStrict, t)) optInfos
    optNames = map fst optInfos
  defRecs <- zip optNames <$>
    mapM (\ (PolyOpt o) -> lift . argDef $ argInfo o) opts
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
  optMatch optNameN optsN (PolyOpt polyOpt) =
    mapM (\ n -> (\ v -> Match (LitP $ StringL n)
    (NormalB $ RecUpdE (VarE optsN) [(mkName $ recName polyOpt, v)]) []) <$>
    [| $procFunc . forceEither $
      (get $(return $ VarE configN) defSection $(return $ VarE optNameN)
      :: Either CPError String) |]) $
    names polyOpt
    where
    procFunc = case argInfo polyOpt of
      NoArgInfo _ _ -> [| const $grab |]
      ReqArgInfo _ _ _ -> grab
      OptArgInfo _ _ _ -> [| $grab . Just |]
    grab = argGrab $ argInfo polyOpt

optToOption :: PolyOpt -> Q Exp
optToOption (PolyOpt opt) =
  [| Option $(lift $ chars opt) $(lift $ names opt) $f $(lift $ help opt) |]
  where
  name = mkName $ recName opt
  f = case argInfo opt of
    NoArgInfo _ _ -> [| NoArg (\ o ->
      -- $(return $ RecUpdE (VarE 'o) [(name, ConE 'True)])) |]
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> grab)) |]
    ReqArgInfo g _ _ -> [| ReqArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| $grab a |])) g |]
    OptArgInfo g _ _ -> [| OptArg (\ a o ->
      $(RecUpdE (VarE 'o) . (:[]) . (,) name <$> [| $grab a |])) g |]
  grab = argGrab $ argInfo opt
