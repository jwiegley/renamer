{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad.IO.Class
import Data.Time
import Fixtures
-- import Hedgehog hiding (Action)
-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range
import Renamer
import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "renamer"
      [ testSimpleRename
      ]

testSimpleRename :: TestTree
testSimpleRename =
  testCase "simple-rename" $
    runWithFixture do
      photo "test/240806_0082.xmp" "2024-08-16T20:59:10.735765931Z"
      photo "test/240806_0082.JPG" "2024-08-16T20:52:16.354628974Z"
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
      photo "test/240806_0081.xmp" "2024-08-16T19:35:40.703006Z"
      runAppT (defaultOptions) do
        details@[f82xmp, f82jpg, f81cr3, f81xmp] <-
          gatherDetails
            True
            [ "test/240806_0082.xmp",
              "test/240806_0082.JPG",
              "test/240806_0081.CR3",
              "test/240806_0081.xmp"
            ]
        renamings <- simpleRenamings utc details
        liftIO $
          renamings
            @?= [ simpleRename
                    f81cr3
                    "240816_0001.cr3"
                    "2024-08-16T19:35:40.702857Z",
                  simpleRename
                    f82jpg
                    "240816_0002.jpg"
                    "2024-08-16T20:52:16.354628974Z"
                ]
        liftIO $
          siblingRenamings details renamings
            @?= [ followBase f81xmp "240816_0001.xmp" "test/240806_0081.CR3",
                  followBase f82xmp "240816_0002.xmp" "test/240806_0082.JPG"
                ]
