module Opts where

import Data.PolyOpt

optDesc :: [PolyOpt]
optDesc = [
  noArg ["verbose"] "v"
    "Print version",
  reqArg ["color","colour"] "c"
    "BYTES" (Nothing :: Maybe String) (Just . read)
    "Foreground color",
  optArg ["show-decimal"] ""
    "N" (0 :: Int) read
    "Show full decimal precision, or to N digits"]
