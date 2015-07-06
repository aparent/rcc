module Circuit
  ( Circuit (..)
  , Gate (..)
  , size
  , writeQC
  ) where

import Data.Vector((!))
import qualified Data.Vector as V

data Circuit =
  Circuit { circIntSize :: Int
          , inputs  :: [ String ]
          , gates   :: [ Gate ]
          } deriving (Show, Eq)

data Gate = Not Int
          | Cnot Int Int
          | Toff Int Int Int
          | Fred Int Int Int
  deriving (Show,Eq)

size :: Circuit -> Int
size circ
  | null (gates circ) = 0
  | otherwise = maximum . map gMaxBit $ gates circ
  where gMaxBit :: Gate -> Int
        gMaxBit (Not a) = a
        gMaxBit (Cnot a b) = max a b
        gMaxBit (Toff a b c) = max a $ max b c
        gMaxBit (Fred a b c) = max a $ max b c

inputSize :: Circuit -> Int
inputSize c = length (inputs c) * circIntSize c

writeQC :: Circuit -> String
writeQC circ = v ++ i ++ o ++ "\nBEGIN\n" ++ gateStr ++ "END"
  where v = ".v" ++ varStr
        i = ".i" ++ varStr
        o = ".o" ++ varStr
        varStr =  concatMap (\x -> ' ': lineToName x) [0..size circ] ++ "\n"
        gateStr = concatMap (\x -> writeGate x ++ "\n") (gates circ)
        inps = V.fromList $ inputs circ
        intSize = circIntSize circ
        lineToName :: Int -> String
        lineToName n
          | n < inputSize circ = (inps ! (n `div` intSize)) ++ "i" ++  show (n `mod` intSize)
          | otherwise = 'a' : show (n - inputSize circ)
        writeGate :: Gate -> String
        writeGate (Not a) = "tof " ++ lineToName a
        writeGate (Cnot a b) = "tof " ++ (unwords . map lineToName) [a,b]
        writeGate (Toff a b c) = "tof " ++ (unwords . map lineToName) [a,b,c]
        writeGate (Fred a b c) = "swap " ++ (unwords . map lineToName) [a,b,c]
