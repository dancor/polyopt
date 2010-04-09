{-# LANGUAGE TemplateHaskell #-}

import Data.PolyOpt
import OptDesc

$(polyOpt optDesc)

main :: IO ()
main = do
  (opts, args) <- getOpts "usage"
  print args
  print $ version opts
  print $ color opts
  print $ showDecimal opts
