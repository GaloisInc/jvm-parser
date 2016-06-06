module Main where

import Control.DeepSeq
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Language.JVM.Parser

testFiles :: IO [FilePath]
testFiles = do
  fs <- getDirectoryContents ("test" </> "support")
  let isClass f = takeExtension f == ".class"
  return (map (\f -> "test" </> "support" </> f) (filter isClass fs))

mkTest :: FilePath -> TestTree
mkTest f = testCase (takeBaseName f) $ do
  cl <- loadClass f
  return (rnf (showClass cl))

main :: IO ()
main = do
  fs <- testFiles
  defaultMain (testGroup "class file parsing" (map mkTest fs))
