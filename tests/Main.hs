{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Class
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "renamer"
      [ testProperty "zipper-unzipper" $ property do
          pure $ () @?= ()
      ]
