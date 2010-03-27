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
