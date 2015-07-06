module Simulate
  ( simulate
  , intToBits
  , bitsToInt
  ) where

import Circuit

import Data.Vector((!),(//),Vector)
import qualified Data.Vector as V
import qualified Data.List as L

simulate :: Circuit -> [(String,Integer)]
simulate circ = zip inp (map bitsToInt $ divs intSize simulatedBits)
  where simulatedBits = V.toList $ L.foldl' simGate  initVec (gates circ)
        initVec = V.fromList $ take (size circ + 1) (repeat False)
        intSize = circIntSize circ
        inp = inputs circ
        divs n bs = take n bs : divs n (drop n bs)

        simGate :: Vector Bool -> Gate -> Vector Bool
        simGate vals g =
          case g of
            Not t -> vals // [(t', not $ vals ! t')]
              where t' = fromInteger t
            Cnot c t -> if vals ! c'
                        then vals // [(t', not $ vals ! t')]
                        else vals
              where (t', c') = (fromInteger t , fromInteger c)
            Toff c1 c2 t -> if vals ! c1' && vals ! c2'
                            then vals // [(t', not $ vals ! t')]
                            else vals
              where (t', c1', c2') = (fromInteger t, fromInteger c1, fromInteger c2)
            Fred c t1 t2 -> if vals ! c'
                            then vals // [(t1', vals ! t2'),(t2', vals ! t1')]
                            else vals
              where (t1', t2', c') = (fromInteger t1, fromInteger t2, fromInteger c)


intToBits :: Integer -> [Bool]
intToBits 0 = []
intToBits i = (if mod i 2 == 0
               then False
               else True)
              : intToBits (div i 2)

bitsToInt :: [Bool] -> Integer
bitsToInt = sum . zipWith (\n b -> if b then n else 0) powersOfTwo
  where powersOfTwo = f 1
        f x = x : f (x*2)
