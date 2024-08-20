{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Time
import Fixtures
import Renamer
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty

-- import Test.Tasty.Hedgehog
-- import Hedgehog hiding (Action)
-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range

main :: IO ()
main =
  defaultMain $
    testGroup
      "renamer"
      [ testGroup
          "simple"
          [ testSameName,
            testNoRename,
            testSimpleRename
          ],
        testGroup
          "follow"
          [ testFollowTime,
            testFollowBase
          ]
      ]

testSameName :: TestTree
testSameName =
  testCase "same" $
    runWithFixture do
      photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
      paths <- allPaths
      runAppT (defaultOptions) do
        details@[f1cr3] <- gatherDetails True paths
        renamings <- simpleRenamings utc details
        liftIO $
          renamings
            @?== [ simpleRename
                     f1cr3
                     "240806_0001.cr3"
                     "2024-08-06T19:35:40.702857Z"
                 ]
        liftIO $ do
          filter (needlessRenaming Nothing) renamings
            @?== [ simpleRename
                     f1cr3
                     "240806_0001.cr3"
                     "2024-08-06T19:35:40.702857Z"
                 ]
          overlappedSources Nothing renamings @?== []
          overlappedTargets Nothing renamings @?== []

testSimpleRename :: TestTree
testSimpleRename =
  testCase "rename" $
    runWithFixture do
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
      paths <- allPaths
      runAppT (defaultOptions) do
        details@[f81cr3] <- gatherDetails True paths
        renamings <- simpleRenamings utc details
        liftIO $ do
          renamings
            @?== [ simpleRename
                     f81cr3
                     "240816_0001.cr3"
                     "2024-08-16T19:35:40.702857Z"
                 ]
          filter (needlessRenaming Nothing) renamings @?== []
          overlappedSources Nothing renamings @?== []
          overlappedTargets Nothing renamings @?== []

testNoRename :: TestTree
testNoRename =
  testCase "none" $
    runWithFixture do
      file "test/240806_0081.xmp"
      paths <- allPaths
      runAppT (defaultOptions) do
        details@[_f81xmp] <- gatherDetails True paths
        renamings <- simpleRenamings utc details
        liftIO $ renamings @?== []

testFollowTime :: TestTree
testFollowTime =
  testCase "time" $
    runWithFixture do
      photo "test/240806_0003.cr3" "2024-08-06T19:35:40.702857Z"
      photo "test/240806_0003.jpg" "2024-08-06T19:35:40.702857Z"
      paths <- allPaths
      runAppT (defaultOptions) do
        details@[f3cr3, f3jpg] <- gatherDetails True paths
        renamings <- simpleRenamings utc details
        liftIO $ do
          renamings
            @?== [ followTime
                     f3cr3
                     "240806_0001.cr3"
                     "240806_0003.jpg",
                   simpleRename
                     f3jpg
                     "240806_0001.jpg"
                     "2024-08-06T19:35:40.702857Z"
                 ]
          filter (needlessRenaming Nothing) renamings @?== []
          overlappedSources Nothing renamings @?== []
          overlappedTargets Nothing renamings @?== []

testFollowBase :: TestTree
testFollowBase =
  testCase "base" $
    runWithFixture do
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
      file "test/240806_0081.xmp"
      photo "test/240806_0082.JPG" "2024-08-16T20:52:16.354628974Z"
      file "test/240806_0082.xmp"
      paths <- allPaths
      runAppT (defaultOptions) do
        details@[f81cr3, f81xmp, f82jpg, f82xmp] <- gatherDetails True paths
        renamings <- simpleRenamings utc details
        liftIO $
          renamings
            @?== [ simpleRename
                     f81cr3
                     "240816_0001.cr3"
                     "2024-08-16T19:35:40.702857Z",
                   simpleRename
                     f82jpg
                     "240816_0002.jpg"
                     "2024-08-16T20:52:16.354628974Z"
                 ]
        liftIO $ do
          let siblings = siblingRenamings details renamings
          siblings
            @?== [ followBase f81xmp "240816_0001.xmp" "test/240806_0081.CR3",
                   followBase f82xmp "240816_0002.xmp" "test/240806_0082.JPG"
                 ]
          filter (needlessRenaming Nothing) (renamings ++ siblings) @?== []
          overlappedSources Nothing (renamings ++ siblings) @?== []
          overlappedTargets Nothing (renamings ++ siblings) @?== []

(@?==) :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
actual @?== expected = assertEqual' "" expected actual

assertEqual' :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertEqual' preface expected actual =
  unless (actual == expected) (assertFailure msg)
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ ppShow expected
        ++ "\n but got: "
        ++ ppShow actual
