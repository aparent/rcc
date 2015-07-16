module Simulate
  ( simulate
  , intToBits
  , bitsToInt
  ) where

import Circuit

import Prelude
import Data.Vector((!),(//),Vector)
import qualified Data.Vector as V
import qualified Data.List as L

simulate :: Circuit -> ([(String,Integer)] , Integer)
simulate circ = (varVals , ancInt)
  where ancInt = bitsToInt $ drop (length inp * intSize) simulatedBits
        varVals = zip inp (map bitsToInt $ divs intSize simulatedBits)
        simulatedBits = V.toList $ L.foldl' simGate  initVec (gates circ)
        initVec = V.fromList $ replicate (size circ + 1) False
        intSize = circIntSize circ
        inp = inputs circ
        divs n bs = take n bs : divs n (drop n bs)

        simGate :: Vector Bool -> Gate -> Vector Bool
        simGate vals g =
          case g of
            Not t -> vals // [(t, not $ vals ! t)]
            Cnot c t -> if vals ! c
                        then vals // [(t, not $ vals ! t)]
                        else vals
            Toff c1 c2 t -> if vals ! c1 && vals ! c2
                            then vals // [(t, not $ vals ! t)]
                            else vals
            Fred c t1 t2 -> if vals ! c
                            then vals // [(t1, vals ! t2),(t2, vals ! t1)]
                            else vals
            Swap t1 t2 -> vals // [(t1, vals ! t2),(t2, vals ! t1)]
            -- TODO Simulation for Hadamard this is not implemented.
            -- For now it just zeros the lines so that it works as it
            -- should in the if statment case
            Hadamard a -> vals // [(a, False)]


intToBits :: Integer -> [Bool]
intToBits 0 = []
intToBits i = (mod i 2 /= 0) : intToBits (div i 2)

bitsToInt :: [Bool] -> Integer
bitsToInt = sum . zipWith (\n b -> if b then n else 0) powersOfTwo
  where powersOfTwo = f 1
        f x = x : f (x*2)
