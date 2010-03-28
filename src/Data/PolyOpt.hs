{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

module Data.PolyOpt (PolyOpt, polyOpt, noArg, reqArg, optArg) where

{-
PolyOpt will allow a no-repetition specification of program options and then
automatically make those options work simultaneously as command line options
and as Data.ConfigFile options.

usage example (currently just have the TH Opts datatype constructor):

Opts.hs:
module Opts where

import Data.PolyOpt

optDesc :: [PolyOpt]
optDesc = [
  noArg ["version"] "v"
    "Print version",
  reqArg ["color","colour"] "c"
    "NAME" (Nothing :: Maybe String) (Just . read)
    "Foreground color",
  optArg ["show-decimal"] ""
    "N" (0 :: Int) read
    "Show full decimal precision, or to N digits"]

Main.hs:
{-# LANGUAGE TemplateHaskell #-}

import Data.PolyOpt
import Opts

$(polyOpt optDesc)

main :: IO ()
main = do
  let
    opts = Opts {
      verbose = True,
      color = Nothing,
      showDecimal = 5}
  putStrLn "hi"
-}

import Control.Applicative
import Data.Char
import Data.Typeable
import Language.Haskell.Meta.Parse
import Language.Haskell.TH

data ArgInfo a = ArgInfo {
  argRequired :: Bool,
  argGloss :: String,
  argDef :: a,
  argRead :: String -> a}

data PolyOptA a = PolyOptA {
  names :: [String],
  chars :: [Char],
  argInfo :: Maybe (ArgInfo a),
  help :: String}

data PolyOpt = forall a. (Typeable a) => PolyOpt (PolyOptA a)

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c h = PolyOpt (PolyOptA n c Nothing h :: PolyOptA ())

reqArg, optArg :: (Typeable a) => [String] -> [Char] -> String -> a ->
  (String -> a) -> String -> PolyOpt
reqArg n c g d r = PolyOpt . PolyOptA n c (Just $ ArgInfo True g d r)
optArg n c g d r = PolyOpt . PolyOptA n c (Just $ ArgInfo False g d r)

-- there might be a TH way to do this that doesn't need Typeable at all..
-- unsure
tOf :: (Typeable a) => a -> Type
tOf x = either error id . parseType $ showsTypeRep (typeOf x) ""

dashToCamel :: String -> String
dashToCamel [] = []
dashToCamel ('-':x:xs) = toUpper x : dashToCamel xs
dashToCamel ['-'] = error "polyOpt: trailing dash in option name not allowed"
dashToCamel (x:xs) = x : dashToCamel xs

-- todo: implement tOf in TH or make all this stuff non-monadic
argToType :: (Typeable a) => ArgInfo a -> Q Type
argToType arg = do
  return . tOf $ argDef arg

optToType :: (Typeable a) => PolyOptA a -> Q Type
optToType = maybe (return $ ConT ''Bool) argToType . argInfo

optToRecord :: (Typeable a) => PolyOptA a -> Q (Name, Strict, Type)
optToRecord opt = (,,)
  (mkName . dashToCamel . head $ names opt)
  NotStrict <$> optToType opt

optBoxToRecord :: PolyOpt -> Q (Name, Strict, Type)
optBoxToRecord (PolyOpt opt) = optToRecord opt

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  optRecords <- mapM optBoxToRecord opts
  return [DataD [] (mkName "Opts") [] [RecC (mkName "Opts") optRecords] []]
