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
