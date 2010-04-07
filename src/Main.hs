{-# LANGUAGE TemplateHaskell #-}

import Data.PolyOpt
import OptDesc

$(polyOpt optDesc)

main :: IO ()
main = do
  let opts = defOpts
  print $ version opts
  print $ color opts
  print $ showDecimal opts
