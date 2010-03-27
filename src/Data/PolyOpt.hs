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
import Language.Haskell.TH

data ArgInfo a = ArgInfo {
  argType :: Type,
  argRequired :: Bool,
  argGloss :: String,
  argDef :: a,
  argRead :: String -> a}

data PolyOptA a = PolyOptA {
  names :: [String],
  chars :: [Char],
  argInfo :: Maybe (ArgInfo a),
  help :: String}

data PolyOpt = forall a. PolyOpt (PolyOptA a)

noArg :: [String] -> [Char] -> String -> PolyOpt
noArg n c = PolyOpt . PolyOptA n c Nothing

reqArg, optArg :: (Typeable a) => [String] -> [Char] -> String -> a ->
  (String -> a) -> String -> PolyOpt
reqArg n c g d r = PolyOpt . PolyOptA n c (Just $ ArgInfo (tOf d) True g d r)
optArg n c g d r = PolyOpt . PolyOptA n c (Just $ ArgInfo (tOf d) False g d r)

-- here be hacks!  i'm terrible at TH, there must be a real way to do it.
-- this hack works for the simple types that happen to be all i need at first,
-- but it is completely unacceptable and must be fixed to call the library
-- usable at all.
tOf :: (Typeable a) => a -> Type
tOf x = foldr1 AppT [ConT $ mkName w | w <- map suicide . words $ showsTypeRep (typeOf x) ""]
  where
    suicide "[Char]" = "String"
    suicide a = a

dashToCamel :: String -> String
dashToCamel [] = []
dashToCamel ('-':x:xs) = toUpper x : dashToCamel xs
dashToCamel ['-'] = error "polyOpt: trailing dash in option name not allowed"
dashToCamel (x:xs) = x : dashToCamel xs

argToType :: ArgInfo a -> Q Type
argToType arg = do
  {-
  -- this is how i imagined not having the tOf hack, but it is illegal for
  -- reasons i don't understand :(
  let
    ArgInfo _ _ d _ = arg
  VarI _ t _ _ <- reify 'd
  return t
  -}
  return $ argType arg

optToType :: PolyOptA a -> Q Type
optToType = maybe (return $ ConT ''Bool) argToType . argInfo

optToRecord :: PolyOptA a -> Q (Name, Strict, Type)
optToRecord opt = (,,)
  (mkName . dashToCamel . head $ names opt)
  NotStrict <$> optToType opt

optBoxToRecord :: PolyOpt -> Q (Name, Strict, Type)
optBoxToRecord (PolyOpt opt) = optToRecord opt

polyOpt :: [PolyOpt] -> Q [Dec]
polyOpt opts = do
  optRecords <- mapM optBoxToRecord opts
  return [DataD [] (mkName "Opts") [] [RecC (mkName "Opts") optRecords] []]
