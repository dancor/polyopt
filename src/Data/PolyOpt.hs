{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

module Data.PolyOpt (PolyOpt, polyOpt, noArg, reqArg, optArg,
  noArgGen, reqArgGen, optArgGen) where

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
    optArgGen ["show-decimal"] ""
      "N" (0 :: Int) (maybe 8 read)
      "Show full decimal precision, or to N digits"]

Main.hs:
  {-# LANGUAGE TemplateHaskell #-}

  import Data.PolyOpt
  -- GHC TH seems to require optDesc in a separate module:
  import OptDesc

  $(polyOpt optDesc)

  main :: IO ()
  main = do
    (opts, args) <- getOpts optDesc "usage"
    print $ verbose opts
    print $ color opts
    print $ showDecimal opts

This is what gets generated:
  data Opts = Opts {
    version :: Bool,
    color :: Maybe String,
    showDecimal :: Int}

  defOpts :: Opts
  defOpts = Opts {
    version = False,
    color = Nothing :: Maybe String,
    showDecimal = 0 :: Int}

  getOpts :: [PolyOpt] -> String -> IO (Opts, [String])
  getOpts optDesc header = do
    let
      options = [
        Option "v" ["version"]
          (NoArg (\ o -> o {version = True}))
          "Print version",
        Option "c" ["color", "colour"]
          (ReqArg (\ a o -> o {color = (Just) a}) "NAME")
          "Foreground color",
        Option "" ["show-decimal"]
          (OptArg (\ a o -> o {showDecimal = (maybe 8 read) a}) "N")
          "Show full decimal precision, or to N digits"]
    args <- getArgs
    return $ case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) defOpts o, n)
      (_, _, e) -> error $ concat e ++ usageInfo header options
-}

import Control.Applicative
import Data.Char
import Data.Typeable
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.Console.GetOpt
import System.Environment

data ArgInfo a =
  NoArgInfo {
    argDef :: a,
    noArgRead :: a} |
  ReqArgInfo {
    argGloss :: String,
    argDef :: a,
    reqArgRead :: String -> a} |
  OptArgInfo {
    argGloss :: String,
    argDef :: a,
    optArgRead :: Maybe String -> a}

data PolyOptA a = PolyOptA {
  names :: [String],
  chars :: [Char],
  argInfo :: ArgInfo a,
  help :: String}

data PolyOpt = forall a. (Lift a, Typeable a) => PolyOpt (PolyOptA a)

noArgGen :: (Lift a, Typeable a) =>
  [String] -> [Char] -> a -> a -> String -> PolyOpt
noArgGen n c d r = PolyOpt . PolyOptA n c (NoArgInfo d r)

reqArgGen :: (Lift a, Typeable a) => [String] -> [Char] -> String -> a ->
  (String -> a) -> String -> PolyOpt
reqArgGen n c g d r = PolyOpt . PolyOptA n c (ReqArgInfo g d r)

optArgGen :: (Lift a, Typeable a) => [String] -> [Char] -> String -> a ->
  (Maybe String -> a) -> String -> PolyOpt
optArgGen n c g d r = PolyOpt . PolyOptA n c (OptArgInfo g d r)

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c = noArgGen n c False True

reqArg :: [String] -> [Char] -> String -> String -> PolyOpt
reqArg n c g = reqArgGen n c g Nothing Just

optArg :: [String] -> [Char] -> String -> String -> PolyOpt
optArg n c g = optArgGen n c g Nothing Just

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

optBoxInfo :: PolyOpt -> (Name, Type)
optBoxInfo (PolyOpt opt) =
  (mkName . dashToCamel . head $ names opt,
  tOf . argDef $ argInfo opt)

defGen (PolyOpt opt) = lift . argDef $ argInfo opt

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  defRecs <- zip optNames <$> mapM defGen opts
  return [
    DataD [] optsN [] [RecC optsN optRecords] [],
    ValD (VarP $ mkName "defOpts") (NormalB $ RecConE optsN defRecs) []
    ]
  where
  optsN = mkName "Opts"
  optInfos = map optBoxInfo opts
  -- is IsStrict better?  i have no idea, probably doesn't matter
  optRecords = map (\ (n, t) -> (n, NotStrict, t)) optInfos
  optNames = map fst optInfos
