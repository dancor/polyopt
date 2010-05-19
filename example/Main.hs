import System.Directory
import System.FilePath
-- Using a separate module allows you to name your options without top-level
-- name clashes.
import qualified Opt

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  {- ~/.lol/config could contain something like this, or not exist:
version = True
color = red
show-decimal = 4
  -}
  (opts, args) <- Opt.getOpts (homeDir </> ".lol" </> "config") $
    "usage: ./lol [options] [args]\n" ++ Opt.optInfo
  print $ Opt.version opts
  print $ Opt.color opts
  print $ Opt.showDecimal opts
  print args
