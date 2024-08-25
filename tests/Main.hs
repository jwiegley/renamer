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
            testFollowBaseAsScenario,
            testRedundantFollow
          ],
        testGroup
          "command"
          [ testImport,
            testImportCollision,
            testImportCollisionSame
          ],
        testGroup
          "scenario"
          []
      ]

-- testScenario "tests/scenario/basic.json"

testSameName :: TestTree
testSameName = testCase "same" $ do
  (Scenario _ _ [f1cr3] _ _ _ srs rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
  srs
    @?== [ forTime
             f1cr3
             "test/240806_0001.cr3"
             "2024-08-06T19:35:40.702857Z"
         ]
  rs @?== []
  paths @?== ["test/240806_0001.cr3"]

testSimpleRename :: TestTree
testSimpleRename = testCase "rename" $ do
  (Scenario _ _ [f81cr3] _ _ _ _ rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
  rs
    @?== [ forTime
             f81cr3
             "test/240816_0001.cr3"
             "2024-08-16T19:35:40.702857Z"
         ]
  paths @?== ["test/240816_0001.cr3"]

testNoRename :: TestTree
testNoRename = testCase "none" $ do
  (Scenario _ _ [_f81xmp] _ _ _ srs _rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      file "test/240806_0081.xmp"
  srs @?== []
  paths @?== ["test/240806_0081.xmp"]

testDateNoDate :: TestTree
testDateNoDate = testCase "dateNoDate" $ do
  (Scenario _ _ [_f1jpg, f2jpg] _ _ _ srs _rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      file "test/240806_0001.jpg"
      photo "test/240806_0002.jpg" "2024-08-06T19:35:40.702857Z"
  srs
    @?== [ forTime
             f2jpg
             "test/240806_0002.jpg"
             "2024-08-06T19:35:40.702857Z"
         ]
  paths @?== ["test/240806_0001.jpg", "test/240806_0002.jpg"]

testFollowTime :: TestTree
testFollowTime = testCase "time" $ do
  (Scenario _ _ [f3cr3, f3jpg] _ _ _ srs _rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/240806_0003.cr3" "2024-08-06T19:35:40.702857Z"
      photo "test/240806_0003.jpg" "2024-08-06T19:35:40.702857Z"
  srs
    @?== [ forTime
             f3cr3
             "test/240806_0001.cr3"
             "2024-08-06T19:35:40.702857Z",
           forTime
             f3jpg
             "test/240806_0001.jpg"
             "2024-08-06T19:35:40.702857Z"
         ]
  paths @?== ["test/240806_0001.cr3", "test/240806_0001.jpg"]

testFollowTimeNoOverlap :: TestTree
testFollowTimeNoOverlap = testCase "timeNoOverlap" $ do
  let ((Scenario _ _ [f24jpg, f24cr2, f134jpg] _ _ _ srs rs _, 0), paths) =
        runSimulation $ do
          photo "test/120404_0024.JPG" "2012-04-04T16:04:50Z"
          photo "test/120404_0024.cr2" "2012-04-04T16:04:50Z"
          photo "test/120404_0134.jpg" "2012-04-04T16:04:50Z"
          renamerNoIdemCheck ["test"] [] Nothing
  srs
    @?== [ forTime
             f24jpg
             "test/120404_0001.jpg"
             "2012-04-04T16:04:50Z",
           forTime
             f24cr2
             "test/120404_0002.cr2"
             "2012-04-04T16:04:50Z",
           forTime
             f134jpg
             "test/120404_0003.jpg"
             "2012-04-04T16:04:50Z"
         ]
  groupRenamingsBy (^. renamingFrom) rs @?== []
  paths
    @?== [ "test/120404_0001.jpg",
           "test/120404_0002.cr2",
           "test/120404_0003.jpg"
         ]
  let ((Scenario _ _ [f1jpg, f2cr2, f3jpg] _ _ _ srs' rs' _, 0), paths') =
        runSimulation $ do
          photo "test/120404_0003.jpg" "2012-04-04T16:04:50Z"
          photo "test/120404_0002.cr2" "2012-04-04T16:04:50Z"
          photo "test/120404_0001.jpg" "2012-04-04T16:04:50Z"
          renamerNoIdemCheck (reverse paths) [] Nothing
  srs'
    @?== [ forTime
             f1jpg
             "test/120404_0001.jpg"
             "2012-04-04T16:04:50Z",
           forTime
             f2cr2
             "test/120404_0002.cr2"
             "2012-04-04T16:04:50Z",
           forTime
             f3jpg
             "test/120404_0003.jpg"
             "2012-04-04T16:04:50Z"
         ]
  groupRenamingsBy (^. renamingFrom) rs' @?== []
  paths'
    @?== [ "test/120404_0001.jpg",
           "test/120404_0002.cr2",
           "test/120404_0003.jpg"
         ]

testFollowBase :: TestTree
testFollowBase = testCase "base" $ do
  (Scenario _ _ [f81xmp, f82xmp, f81cr3, f82jpg] _ _ _ srs _rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
      file "test/240806_0081.xmp"
      photo "test/240806_0082.JPG" "2024-08-16T20:52:16.354628974Z"
      file "test/240806_0082.xmp"
  srs
    @?== [ forTime
             f81cr3
             "test/240816_0001.cr3"
             "2024-08-16T19:35:40.702857Z",
           forBase f81xmp "test/240816_0001.xmp" "test/240806_0081",
           forTime
             f82jpg
             "test/240816_0002.jpg"
             "2024-08-16T20:52:16.354628974Z",
           forBase f82xmp "test/240816_0002.xmp" "test/240806_0082"
         ]
  filter idempotentRenaming srs @?== []
  groupRenamingsBy (^. renamingFrom) srs @?== []
  groupRenamingsBy (^. renamingTo) srs @?== []
  paths
    @?== [ "test/240816_0001.cr3",
           "test/240816_0001.xmp",
           "test/240816_0002.jpg",
           "test/240816_0002.xmp"
         ]

testFollowBaseAsScenario :: TestTree
testFollowBaseAsScenario = testCase "base" $ do
  ((Scenario _ _ ds@[f81xmp, f82xmp, f81cr3, f82jpg] _ _ _ _ _ _), paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/240806_0081.CR3" "2024-08-16T19:35:40.702857Z"
      file "test/240806_0081.xmp"
      photo "test/240806_0082.JPG" "2024-08-16T20:52:16.354628974Z"
      file "test/240806_0082.xmp"
  confirmScenario
    Scenario
      { _scenarioPid = 123,
        _scenarioTimeZoneMinutes = 0,
        _scenarioRepository = ds,
        _scenarioDestination = Nothing,
        _scenarioInputs = [],
        _scenarioPhotoGroups =
          [ Right
              ( PhotoGroup
                  ( (f81cr3, ForTime (time "2024-08-16T19:35:40.702857Z"))
                      :| [(f81xmp, ForBase "test/240806_0081")]
                  )
                  (Prefix "test/240816")
              ),
            Right
              ( PhotoGroup
                  ( (f82jpg, ForTime (time "2024-08-16T20:52:16.354628974Z"))
                      :| [(f82xmp, ForBase "test/240806_0082")]
                  )
                  (Prefix "test/240816")
              )
          ],
        _scenarioSimpleRenamings =
          [ forTime
              f81cr3
              "test/240816_0001.cr3"
              "2024-08-16T19:35:40.702857Z",
            forBase f81xmp "test/240816_0001.xmp" "test/240806_0081",
            forTime
              f82jpg
              "test/240816_0002.jpg"
              "2024-08-16T20:52:16.354628974Z",
            forBase f82xmp "test/240816_0002.xmp" "test/240806_0082"
          ],
        _scenarioRenamings =
          [ forTime
              f81cr3
              "test/240816_0001.cr3"
              "2024-08-16T19:35:40.702857Z",
            forBase f81xmp "test/240816_0001.xmp" "test/240806_0081",
            forTime
              f82jpg
              "test/240816_0002.jpg"
              "2024-08-16T20:52:16.354628974Z",
            forBase f82xmp "test/240816_0002.xmp" "test/240806_0082"
          ],
        _scenarioMappings =
          [ forTime
              f81cr3
              "test/240816_0001.cr3"
              "2024-08-16T19:35:40.702857Z",
            forBase f81xmp "test/240816_0001.xmp" "test/240806_0081",
            forTime
              f82jpg
              "test/240816_0002.jpg"
              "2024-08-16T20:52:16.354628974Z",
            forBase f82xmp "test/240816_0002.xmp" "test/240806_0082"
          ]
      }
  paths
    @?== [ "test/240816_0001.cr3",
           "test/240816_0001.xmp",
           "test/240816_0002.jpg",
           "test/240816_0002.xmp"
         ]

testRedundantFollow :: TestTree
testRedundantFollow = testCase "redundant" $ do
  (Scenario _ _ [f2heic, f2jpg] _ _ _ _srs rs _, paths) <-
    renamer ["test"] [] Nothing $ do
      photo "test/230528_0002.heic" "2024-08-16T19:35:40.702857Z"
      photo "test/230528_0002.jpg" "2024-08-16T19:35:40.702857Z"
  rs
    @?== [ forTime
             f2heic
             "test/240816_0001.heic"
             "2024-08-16T19:35:40.702857Z",
           forTime
             f2jpg
             "test/240816_0001.jpg"
             "2024-08-16T19:35:40.702857Z"
         ]
  paths @?== ["test/240816_0001.heic", "test/240816_0001.jpg"]

testImport :: TestTree
testImport = testCase "import" $ do
  paths <- importer ["test"] ["incoming"] "test" $ do
    photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
    photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
    photo "incoming/IMG_001.jpg" "2024-08-07T20:30:40.702857Z"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240807_0001.jpg"
         ]

testImportCollision :: TestTree
testImportCollision = testCase "collision" $ do
  paths <- importer ["test"] ["incoming"] "test" $ do
    photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
    photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
    photo "incoming/IMG_001.jpg" "2024-08-06T20:30:40.702857Z"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240806_0002.jpg"
         ]

testImportCollisionSame :: TestTree
testImportCollisionSame = testCase "collisionSame" $ do
  paths <- importer ["test"] ["incoming"] "test" $ do
    photo "test/240806_0001.cr3" "2024-08-06T19:35:40.702857Z"
    photo "test/240806_0001.jpg" "2024-08-06T19:35:40.702857Z"
    photo "incoming/IMG_001.cr3" "2024-08-06T20:30:40.702857Z"
    photo "incoming/IMG_002.jpg" "2024-08-06T20:30:40.702857Z"
  paths
    @?== [ "test/240806_0001.cr3",
           "test/240806_0001.jpg",
           "test/240806_0002.cr3",
           "test/240806_0002.jpg"
         ]
