module Main (main) where

import Test.Tasty

import qualified Test.Invalid
import qualified Test.Sanity

main :: IO ()
main = defaultMain $ testGroup "TestTypeLet" [
      Test.Invalid.tests
    , Test.Sanity.tests
    ]
