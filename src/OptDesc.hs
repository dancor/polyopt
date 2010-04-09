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
