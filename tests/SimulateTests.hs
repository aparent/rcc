module SimulateTests
(
  propToBitsFromBitsId
) where

import Simulate
import Test.QuickCheck

propToBitsFromBitsId :: Positive Integer -> Bool
propToBitsFromBitsId (Positive x) = x == bitsToInt (intToBits x)
