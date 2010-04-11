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
