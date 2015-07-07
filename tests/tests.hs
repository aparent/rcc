module Main (main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import SimulateTests
import CompileTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "Simulater"
    [
      testProperty "Converting to and from bits identity" propToBitsFromBitsId
    ] ,
    testGroup "Compiler"
    [
      testCase "Simple simulation test" simpleTestSim
    ]
  ]
