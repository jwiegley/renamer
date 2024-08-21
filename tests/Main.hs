{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty (..))
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
            testFollowTimeNoOverlap,
            testFollowBase,
            testRedundantFollow
          ],
        testGroup
          "command"
          [ testImport,
            testImportCollision,
            testImportCollisionSame
          ]
      ]

testSameName :: TestTree
testSameName = testCase "same" $ runWithFixture do
  photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
  paths <-
    renamer
      ["test"]
      ( \[f1cr3] renamings -> do
          renamings
            @?== [ simpleRename
                     f1cr3
                     "240806_0001.cr3"
                     "2024-08-06T19:35:40.702857Z"
                 ]
          filter (idempotentRenaming Nothing) renamings
            @?== [ simpleRename
                     f1cr3
                     "240806_0001.cr3"
                     "2024-08-06T19:35:40.702857Z"
                 ]
      )
      (\_ _ -> pure ())
      (\_ _ -> pure ())
  paths @?== ["test/240806_0001.cr3"]

testSimpleRename :: TestTree
testSimpleRename = testCase "rename" $ runWithFixture do
  photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
  paths <-
    renamer
      ["test"]
      ( \[f81cr3] ->
          ( @?==
              [ simpleRename
                  f81cr3
                  "240816_0001.cr3"
                  "2024-08-16T19:35:40.702857Z"
              ]
          )
      )
      (\_ _ -> pure ())
      (\_ _ -> pure ())
  paths @?== ["test/240816_0001.cr3"]

testNoRename :: TestTree
testNoRename = testCase "none" $ runWithFixture do
  file "test/240806_0081.xmp"
  paths <-
    renamer
      ["test"]
      ( \[_f81xmp] ->
          ( @?==
              []
          )
      )
      (\_ _ -> pure ())
      (\_ _ -> pure ())
  paths @?== ["test/240806_0081.xmp"]

testFollowTime :: TestTree
testFollowTime = testCase "time" $ runWithFixture do
  photo "test/240806_0003.cr3" "2024-08-06T19:35:40.702857Z"
  photo "test/240806_0003.jpg" "2024-08-06T19:35:40.702857Z"
  paths <-
    renamer
      ["test"]
      ( \[f3cr3, f3jpg] ->
          ( @?==
              [ followTime
                  f3cr3
                  "240806_0001.cr3"
                  "240806_0003.jpg",
                simpleRename
                  f3jpg
                  "240806_0001.jpg"
                  "2024-08-06T19:35:40.702857Z"
              ]
          )
      )
      (\_ _ -> pure ())
      (\_ _ -> pure ())
  paths @?== ["test/240806_0001.cr3", "test/240806_0001.jpg"]

testFollowTimeNoOverlap :: TestTree
testFollowTimeNoOverlap = testCase "timeNoOverlap" $ runWithFixture do
  photo "test/120404_0024.JPG" "2012-04-04T16:04:50Z"
  photo "test/120404_0024.cr2" "2012-04-04T16:04:50Z"
  photo "test/120404_0134.jpg" "2012-04-04T16:04:50Z"
  paths <-
    renamer
      ["test"]
      ( \[f24jpg, f24cr2, f134jpg] ->
          ( @?==
              [ simpleRenameAvoidOverlap
                  f24jpg
                  "120404_0003.jpg"
                  "2012-04-04T16:04:50Z",
                simpleRenameAvoidOverlap
                  f24cr2
                  "120404_0002.cr2"
                  "2012-04-04T16:04:50Z",
                simpleRenameAvoidOverlap
                  f134jpg
                  "120404_0001.jpg"
                  "2012-04-04T16:04:50Z"
              ]
          )
      )
      (\_ _ -> pure ())
      ( \[f24jpg, f24cr2, _f134jpg] renamings ->
          overlappedRenamings (^. source) renamings
            @?== [ simpleRenameAvoidOverlap
                     f24jpg
                     "120404_0003.jpg"
                     "2012-04-04T16:04:50Z"
                     :| [ followBase
                            f24jpg
                            "120404_0002.jpg"
                            "test/120404_0024.cr2"
                        ],
                   simpleRenameAvoidOverlap
                     f24cr2
                     "120404_0002.cr2"
                     "2012-04-04T16:04:50Z"
                     :| [ followBase
                            f24cr2
                            "120404_0003.cr2"
                            "test/120404_0024.JPG"
                        ]
                 ]
      )
  paths
    @?== [ "test/120404_0001.jpg",
           "test/120404_0002.cr2",
           "test/120404_0003.jpg"
         ]

testFollowBase :: TestTree
testFollowBase = testCase "base" $ runWithFixture do
  photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
  file "test/240806_0081.xmp"
  photo "test/240806_0082.JPG" "2024-08-16T20:52:16.354628974Z"
  file "test/240806_0082.xmp"
  paths <-
    renamer
      ["test"]
      ( \[f81cr3, _f81xmp, f82jpg, _f82xmp] ->
          ( @?==
              [ simpleRename
                  f81cr3
                  "240816_0001.cr3"
                  "2024-08-16T19:35:40.702857Z",
                simpleRename
                  f82jpg
                  "240816_0002.jpg"
                  "2024-08-16T20:52:16.354628974Z"
              ]
          )
      )
      ( \[_f81cr3, f81xmp, _f82jpg, f82xmp] ->
          ( @?==
              [ followBase f81xmp "240816_0001.xmp" "test/240806_0081.CR3",
                followBase f82xmp "240816_0002.xmp" "test/240806_0082.JPG"
              ]
          )
      )
      ( \_ renamings -> do
          filter (idempotentRenaming Nothing) renamings @?== []
          overlappedRenamings (^. source) renamings @?== []
          overlappedRenamings (target Nothing) renamings @?== []
      )
  paths
    @?== [ "test/240816_0001.cr3",
           "test/240816_0001.xmp",
           "test/240816_0002.jpg",
           "test/240816_0002.xmp"
         ]

testRedundantFollow :: TestTree
testRedundantFollow = testCase "redundant" $ runWithFixture do
  photo "test/230528_0002.heic" "2024-08-16T19:35:40.702857Z"
  photo "test/230528_0002.jpg" "2024-08-16T19:35:40.702857Z"
  paths <-
    renamer
      ["test"]
      ( \[f2heic, f2jpg] ->
          ( @?==
              [ followTime
                  f2heic
                  "240816_0001.heic"
                  "230528_0002.jpg",
                simpleRename
                  f2jpg
                  "240816_0001.jpg"
                  "2024-08-16T19:35:40.702857Z"
              ]
          )
      )
      ( \[f2heic, f2jpg] ->
          ( @?==
              [ followBase f2jpg "240816_0001.jpg" "test/230528_0002.heic",
                followBase f2heic "240816_0001.heic" "test/230528_0002.jpg"
              ]
          )
      )
      ( \[f2heic, f2jpg] renamings -> do
          filter (idempotentRenaming Nothing) renamings @?== []
          overlappedRenamings (^. source) renamings
            @?== [ followTime
                     f2heic
                     "240816_0001.heic"
                     "230528_0002.jpg"
                     :| [ followBase
                            f2heic
                            "240816_0001.heic"
                            "test/230528_0002.jpg"
                        ],
                   simpleRename
                     f2jpg
                     "240816_0001.jpg"
                     "2024-08-16T19:35:40.702857Z"
                     :| [ followBase
                            f2jpg
                            "240816_0001.jpg"
                            "test/230528_0002.heic"
                        ]
                 ]
          removeRedundantRenamings (^. source) Nothing renamings
            @?== [ followTime
                     f2heic
                     "240816_0001.heic"
                     "230528_0002.jpg",
                   simpleRename
                     f2jpg
                     "240816_0001.jpg"
                     "2024-08-16T19:35:40.702857Z"
                 ]
          overlappedRenamings (target Nothing) renamings
            @?== [ followTime
                     f2heic
                     "240816_0001.heic"
                     "230528_0002.jpg"
                     :| [ followBase
                            f2heic
                            "240816_0001.heic"
                            "test/230528_0002.jpg"
                        ],
                   simpleRename
                     f2jpg
                     "240816_0001.jpg"
                     "2024-08-16T19:35:40.702857Z"
                     :| [ followBase
                            f2jpg
                            "240816_0001.jpg"
                            "test/230528_0002.heic"
                        ]
                 ]
          removeRedundantRenamings (target Nothing) Nothing renamings
            @?== [ followTime
                     f2heic
                     "240816_0001.heic"
                     "230528_0002.jpg",
                   simpleRename
                     f2jpg
                     "240816_0001.jpg"
                     "2024-08-16T19:35:40.702857Z"
                 ]
      )
  -- Due to the overlapped sources and targets, no renaming was able to occur.
  paths @?== ["test/240816_0001.heic", "test/240816_0001.jpg"]

testImport :: TestTree
testImport = testCase "import" $ runWithFixture do
  photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
  photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
  photo "incoming/IMG_001.jpg" "2024-08-07T20:30:40.702857Z"
  paths <- importer ["test"] ["incoming"] "test"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240807_0001.jpg"
         ]

testImportCollision :: TestTree
testImportCollision = testCase "collision" $ runWithFixture do
  photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
  photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
  photo "incoming/IMG_001.jpg" "2024-08-06T20:30:40.702857Z"
  paths <- importer ["test"] ["incoming"] "test"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240806_0002.jpg"
         ]

testImportCollisionSame :: TestTree
testImportCollisionSame = testCase "collisionSame" $ runWithFixture do
  photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
  photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
  photo "incoming/IMG_001.cr3" "2024-08-06T20:30:40.702857Z"
  photo "incoming/IMG_002.jpg" "2024-08-06T20:30:40.702857Z"
  paths <- importer ["test"] ["incoming"] "test"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240806_0002.cr3",
           "test/240806_0002.jpg"
         ]

(@?==) :: (Eq a, Show a, HasCallStack, MonadIO m) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

assertEqual' :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertEqual' preface expected actual =
  unless (actual == expected) $ assertFailure msg
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ ppShow expected
        ++ "\n but got: "
        ++ ppShow actual
