module CompileTests
(
  simpleTestSim
, propAncillaZero
, propAncillaZeroOpt
) where

import Test.HUnit

import ParseJanus
import GenJanus
import CircuitOptimize
import Simulate

simpleProg :: String
simpleProg =
  unlines
  [ "x1 x2;"
  , "x1 += 5;"
  , "x2 += 2;"
  , "x1 += x2;" ]


propAncillaZero :: Janus -> Bool
propAncillaZero = (== 0) . snd . simulate . genJanus 5

propAncillaZeroOpt :: Janus -> Bool
propAncillaZeroOpt = (== 0) . snd . simulate . optimize . genJanus 5

testProgSim :: String -> ([(String,Integer)],Integer) -> Assertion
testProgSim prog expected =
  case parseValue of
    Left pe -> assertFailure $ show pe
    Right jan -> expected  @=? (simulate . genJanus 3 $ jan)
  where parseValue = parseJanus "" prog

simpleTestSim :: Assertion
simpleTestSim = testProgSim simpleProg ([("x1",7),("x2",2)],0)
