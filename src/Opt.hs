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
    "N" (0 :: Int) [| maybe 8 read |]
    "Show full decimal precision, or to N digits"])