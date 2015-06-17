module Circuit
  ( Circuit (..)
  , Gate (..)
  , size
  ) where

data Circuit = Circuit {
        inputs  :: [ (String,[Integer]) ]
      , outputs :: [ (String,[Integer]) ]
      , gates   :: [Gate]
} deriving Show

data Gate = Not Integer
          | Cnot Integer Integer
          | Toff Integer Integer Integer
  deriving Show

size :: Circuit -> Integer
size = maximum . map gMaxBit . gates

gMaxBit :: Gate -> Integer
gMaxBit (Not a) = a
gMaxBit (Cnot a b) = max a b
gMaxBit (Toff a b c) = max a $ max b c
