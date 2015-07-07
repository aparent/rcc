module CompileTests
(
  simpleTestSim
) where

import Test.HUnit

import ParseJanus
import GenJanus
import Simulate

simpleProg :: String
simpleProg =
  unlines
  [ "x1 x2;"
  , "x1 += 5;"
  , "x2 += 2;"
  , "x1 += x2;" ]


testProgSim :: String -> ([(String,Integer)],Integer) -> Assertion
testProgSim prog expected =
  case parseValue of
    Left pe -> assertFailure $ show pe
    Right jan -> expected  @=? (simulate . genJanus 3 $ jan)
  where parseValue = parseJanus "" prog

simpleTestSim :: Assertion
simpleTestSim = testProgSim simpleProg ([("x1",7),("x2",2)],0)
