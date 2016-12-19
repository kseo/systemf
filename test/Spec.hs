import Language.LambdaCalculus

import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath
import System.FilePath.Glob

import Test.Tasty
import Test.Tasty.Golden as G

main :: IO ()
main = do
  paths <- listTestFiles
  goldens <- mapM mkGoldenTest paths
  defaultMain (testGroup "Tests" goldens)

listTestFiles :: IO [FilePath]
listTestFiles = globDir1 pat "test/tests"
  where pat = compile "*.lc"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  return (goldenVsString testName goldenPath action)
  where
    action :: IO BS.ByteString
    action = do
      script <- readFile path
      let actual = either id id (run script)
      return (BS.pack actual)
