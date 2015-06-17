module Circuit
  ( Circuit (..)
  , Gate (..)
  , size
  ) where

data Circuit = Circuit {
        inputs  :: [ ([Integer], String) ]
      , outputs :: [ ([Integer], String) ]
      , gates   :: [Gate]
}

data Gate = Gate {
    name :: String
  , bits :: [Integer]
}

size :: Circuit -> Integer
size = maximum . map (maximum . bits) . gates
