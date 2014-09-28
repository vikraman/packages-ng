import           Control.Concurrent.Async (mapConcurrently)
import           System.Directory         (setCurrentDirectory)
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))
import           System.FilePath.Glob     (namesMatching)

import           PackagesNG.ParsePackage  (printPackage)

main :: IO ()
main = do
  args <- getArgs
  let repo = case args of
             []       -> "/usr/portage"
             path : _ -> path
  setCurrentDirectory $ repo </> "metadata/md5-cache"
  allFiles <- namesMatching "*/*"
  _ <- printPackage `mapConcurrently` allFiles
  return ()
