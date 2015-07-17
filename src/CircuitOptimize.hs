module CircuitOptimize
  ( optimize
  ) where



import Prelude
import Data.List

import Circuit

optimize :: Circuit -> Circuit
optimize c = c { gates = cancelGates (gates c) }

gateLines :: Gate -> [Int]
gateLines g =
  case g of
    Not n -> [n]
    Cnot n1 n2 -> [n1,n2]
    Toff n1 n2 n3 -> [n1,n2,n3]
    Fred n1 n2 n3 -> [n1,n2,n3]
    Swap n1 n2 -> [n1,n2]
    Hadamard n -> [n]

--Add more cases to this:
--targets commute with targets
--controls commute with controls
commutes :: Gate -> Gate -> Bool
commutes g1 g2 = null $ gateLines g1 `intersect` gateLines g2

cancelGates :: [Gate] -> [Gate]
cancelGates [] = []
cancelGates (g:gs) =
  case elemIndex g gs of
    Nothing -> g : cancelGates gs
    Just n | all (commutes g) (take n gs) -> cancelGates $ delete g gs
           | otherwise -> g : cancelGates gs
