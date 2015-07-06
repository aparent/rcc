module Main (main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
--import Test.Framework.Providers.HUnit

import SimulateTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
          testGroup "Simulate"
              [
                testProperty "Converting to and from bits identity" propToBitsFromBitsId
              ]
    ]
