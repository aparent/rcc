{-# LANGUAGE NoMonomorphismRestriction #-}
module CircuitDiagram (circuitToSvg) where

import Prelude
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts

import Data.List (foldl')

import Circuit

targetRad, charSize, ctrlRad, colSpace :: Double
targetRad = 0.5
charSize = 1
ctrlRad = 0.2
colSpace = 0.2



-- | Takes a circuit, filename, and width then writes out a
-- diagram of the circuit as an SVG
circuitToSvg :: Circuit -> String -> Double -> IO ()
circuitToSvg c f w = renderSVG f (mkWidth w) $ drawCirc c

drawCirc :: Circuit -> Diagram B
drawCirc c = hsep 0.0 [ txt
                      , gs # centerX <> ls
                      , txt
                      ]
  where gs = drawGates (gates c)
        ls = mconcat . map (mkLine.fromIntegral) $ [0..Circuit.size c]
          where mkLine y = hrule 1
                         # lw thin
                         # lc grey
                         # sizedAs gs
                         # translateY y
        txt = mconcat . zipWith placeText (lineNames c) $ map fromIntegral [0..length (lineNames c)]
          where placeText s y = (mkText s <> phantom (rect 4 1 :: D V2 Double))
                              # translateY y
                mkText s = text s
                         # fontSize (local 0.5)
        drawGates = hsep colSpace . map (mconcat . map drawGate) . getDrawCols
        drawGate g =
          case g of
            Not  t -> drawCnot (fromIntegral t) []
            Cnot c1 t -> drawCnot (fromIntegral t) [fromIntegral c1]
            Toff c1 c2 t -> drawCnot (fromIntegral t) $ map fromIntegral [c1,c2]
            Fred c1 t1 t2 -> drawSwap (fromIntegral t1) (fromIntegral t2) [fromIntegral c1]
            Hadamard n -> drawH (fromIntegral n)

drawChar :: Char -> Diagram B
drawChar c = strokeP (textSVG' (TextOpts bit INSIDE_H KERN False charSize charSize) [c])
             # lw 0.0
             # fc black

drawH :: Double -> Diagram B
drawH t = (symb <> base)  # translateY t
  where symb = drawChar 'H'
        base = square (1.7*targetRad)
             # lw thin
             # fc white


drawCnot :: Double -> [Double] -> Diagram B
drawCnot t cs =  circle targetRad # lw thin  # translateY t
              <> controls
              <> line
  where line = drawLine 0 (top - bottom)
             # translateY bottom
          where top = if maxY == t
                      then maxY + targetRad
                      else maxY
                bottom = if minY == t
                         then minY - targetRad
                         else minY
                maxY = maximum (t:cs)
                minY = minimum (t:cs)
        controls = mconcat $ map drawCtrl cs

drawCtrl :: Double -> Diagram B
drawCtrl y = circle ctrlRad # fc black # translateY y

drawSwap :: Double -> Double -> [Double] -> Diagram B
drawSwap t1 t2 cs = targ # translateY t1
                 <> targ # translateY t2
                 <> controls
                 <> line
  where targ =     drawLine 1 1  # center
                <> drawLine 1 (-1) # center
        line = drawLine 0 (top - bottom)
             # translateY bottom
          where top = maximum (t1:t2:cs)
                bottom = minimum (t1:t2:cs)
        controls = mconcat $ map drawCtrl cs

drawLine :: Double -> Double -> Diagram B
drawLine x y = fromSegments [straight $ r2(x,y) ] # lw thin

lineNames :: Circuit -> [String]
lineNames circ = concatMap inputStrings $ inputs circ
  where inputStrings inp = map (\y -> inp ++ "[" ++ show y ++ "]") [0..circIntSize circ - 1]

-- | Groups a list of gates such that each group can be drawn without
-- overlap in the same column
getDrawCols :: [Gate] -> [[Gate]]
getDrawCols = (\(x,y) -> x ++ [y]) . foldl' colFold ([[]],[])
  where colFold (res,curr) next =
          if fits (map gateToRange curr) (gateToRange next)
          then (res, next : curr)
          else (res ++ [curr], [next])
        gateToRange g =
          case g of
            Not n -> (n,n)
            Cnot n1 n2 -> (max n1 n2 , min n1 n2)
            Toff n1 n2 n3 -> (maximum [n1, n2, n3] , minimum [n1, n2, n3])
            Fred n1 n2 n3 -> (maximum [n1, n2, n3] , minimum [n1, n2, n3])
            Hadamard n -> (n,n)
        fits [] _ = True
        fits rs r = all (notInRange r) rs
          where notInRange x y = minT x > maxT y || maxT x < minT y
                maxT = uncurry max
                minT = uncurry min
