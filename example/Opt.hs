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
    "Use full or N digit decimal precision"])
