{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Fixtures
import Renamer
import Test.Tasty
import Test.Tasty.HUnit

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
            testSimpleRename,
            testDateNoDate
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
  (Scenario _ [f1cr3] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ simpleRename
             f1cr3
             "240806_0001.cr3"
             "2024-08-06T19:35:40.702857Z"
         ]
  filter (idempotentRenaming Nothing) (rs ^. allSimpleRenamings)
    @?== [ simpleRename
             f1cr3
             "240806_0001.cr3"
             "2024-08-06T19:35:40.702857Z"
         ]
  paths @?== ["test/240806_0001.cr3"]

testSimpleRename :: TestTree
testSimpleRename = testCase "rename" $ runWithFixture do
  photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
  (Scenario _ [f81cr3] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ simpleRename
             f81cr3
             "240816_0001.cr3"
             "2024-08-16T19:35:40.702857Z"
         ]
  paths @?== ["test/240816_0001.cr3"]

testNoRename :: TestTree
testNoRename = testCase "none" $ runWithFixture do
  file "test/240806_0081.xmp"
  (Scenario _ [_f81xmp] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings) @?== []
  paths @?== ["test/240806_0081.xmp"]

testDateNoDate :: TestTree
testDateNoDate = testCase "dateNoDate" $ runWithFixture do
  file "test/240806_0001.jpg"
  photo "test/240806_0002.jpg" "2024-08-06T19:35:40.702857Z"
  (Scenario _ [_f1jpg, f2jpg] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ simpleRename
             f2jpg
             "240806_0001.jpg"
             "2024-08-06T19:35:40.702857Z"
         ]
  paths @?== ["test/240806_0001.jpg"]

testFollowTime :: TestTree
testFollowTime = testCase "time" $ runWithFixture do
  photo "test/240806_0003.cr3" "2024-08-06T19:35:40.702857Z"
  photo "test/240806_0003.jpg" "2024-08-06T19:35:40.702857Z"
  (Scenario _ [f3cr3, f3jpg] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ followTime
             f3cr3
             "240806_0001.cr3"
             "240806_0003.jpg",
           simpleRename
             f3jpg
             "240806_0001.jpg"
             "2024-08-06T19:35:40.702857Z"
         ]
  paths @?== ["test/240806_0001.cr3", "test/240806_0001.jpg"]

testFollowTimeNoOverlap :: TestTree
testFollowTimeNoOverlap = testCase "timeNoOverlap" $ runWithFixture do
  photo "test/120404_0024.JPG" "2012-04-04T16:04:50Z"
  photo "test/120404_0024.cr2" "2012-04-04T16:04:50Z"
  photo "test/120404_0134.jpg" "2012-04-04T16:04:50Z"
  (Scenario _ [f24jpg, f24cr2, f134jpg] _ _ _ rs _, paths) <-
    renamerNoIdemCheck ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ simpleRenameAvoidOverlap
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
  groupRenamingsBy
    (^. source)
    ( rs ^. allSimpleRenamings
        ++ rs ^. allSiblingRenamings
    )
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
  paths
    @?== [ "test/120404_0001.jpg",
           "test/120404_0002.cr2",
           "test/120404_0003.jpg"
         ]
  (Scenario _ [f3jpg, f3cr2, f1jpg] _ _ _ rs' _, paths') <-
    renamerNoIdemCheck (reverse paths) [] Nothing
  (rs' ^. allSimpleRenamings)
    @?== [ simpleRenameAvoidOverlap
             f3jpg
             "120404_0003.jpg"
             "2012-04-04T16:04:50Z",
           simpleRenameAvoidOverlap
             f3cr2
             "120404_0002.cr2"
             "2012-04-04T16:04:50Z",
           simpleRenameAvoidOverlap
             f1jpg
             "120404_0001.jpg"
             "2012-04-04T16:04:50Z"
         ]
  groupRenamingsBy
    (^. source)
    (rs' ^. allSimpleRenamings ++ rs' ^. allSiblingRenamings)
    @?== []
  paths'
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
  (Scenario _ _ _ _ [f81cr3, f81xmp, f82jpg, f82xmp] rs _, paths) <-
    renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ simpleRename
             f81cr3
             "240816_0001.cr3"
             "2024-08-16T19:35:40.702857Z",
           simpleRename
             f82jpg
             "240816_0002.jpg"
             "2024-08-16T20:52:16.354628974Z"
         ]
  (rs ^. allSiblingRenamings)
    @?== [ followBase f81xmp "240816_0001.xmp" "test/240806_0081.CR3",
           followBase f82xmp "240816_0002.xmp" "test/240806_0082.JPG"
         ]
  filter (idempotentRenaming Nothing) (rs ^. allRenamingsWithoutRedundancies)
    @?== []
  groupRenamingsBy (^. source) (rs ^. allRenamingsWithoutRedundancies)
    @?== []
  groupRenamingsBy (target Nothing) (rs ^. allRenamingsWithoutRedundancies)
    @?== []
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
  (Scenario _ [f2heic, f2jpg] _ _ _ rs _, paths) <- renamer ["test"] [] Nothing
  (rs ^. allSimpleRenamings)
    @?== [ followTime
             f2heic
             "240816_0001.heic"
             "230528_0002.jpg",
           simpleRename
             f2jpg
             "240816_0001.jpg"
             "2024-08-16T19:35:40.702857Z"
         ]
  (rs ^. allSiblingRenamings)
    @?== [ followBase f2jpg "240816_0001.jpg" "test/230528_0002.heic",
           followBase f2heic "240816_0001.heic" "test/230528_0002.jpg"
         ]
  filter
    (idempotentRenaming Nothing)
    (rs ^. allRenamingsWithoutRedundancies)
    @?== []
  groupRenamingsBy
    (^. source)
    (rs ^. allSimpleRenamings ++ rs ^. allSiblingRenamings)
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
  removeRedundantRenamings
    (^. source)
    Nothing
    (rs ^. allSimpleRenamings ++ rs ^. allSiblingRenamings)
    @?== [ followTime
             f2heic
             "240816_0001.heic"
             "230528_0002.jpg",
           simpleRename
             f2jpg
             "240816_0001.jpg"
             "2024-08-16T19:35:40.702857Z"
         ]
  groupRenamingsBy
    (target Nothing)
    (rs ^. allSimpleRenamings ++ rs ^. allSiblingRenamings)
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
  removeRedundantRenamings
    (target Nothing)
    Nothing
    (rs ^. allSimpleRenamings ++ rs ^. allSiblingRenamings)
    @?== [ followTime
             f2heic
             "240816_0001.heic"
             "230528_0002.jpg",
           simpleRename
             f2jpg
             "240816_0001.jpg"
             "2024-08-16T19:35:40.702857Z"
         ]
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
