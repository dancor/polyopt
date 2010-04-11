import Data.PolyOpt
import qualified Opt

main :: IO ()
main = do
  (opts, args) <- Opt.getOpts "lol.config" "usage"
  print args
  print $ Opt.version opts
  print $ Opt.color opts
  print $ Opt.showDecimal opts
