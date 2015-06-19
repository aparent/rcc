module GenJanus (genJanus) where

import ParseJanus
import Circuit

import Control.Monad.State
import Control.Exception
import Data.Maybe
import qualified Data.Map as Map

genJanus :: Int -> Janus ->  Circuit 
genJanus intSize (decl,stmt) = 
  Circuit { circIntSize = intSize
          , inputs = decl
          , gates = mkGates }
  where inputs = zip decl  $ f [0..]
        f ls = take intSize ls : f (drop intSize ls)
        mkGates = genStmt (fromIntegral intSize) (fromIntegral ancInd) inputs stmt
        ancInd = intSize * length inputs

type ExprState = State ([Integer],[Gate])

genStmt :: Integer -> Integer -> [(String,[Integer])] -> Stmt -> [Gate]
genStmt intSize ancInd vmap stmt = 
  case stmt of
    ModStmt v o e -> handleOp v o e
    ModIndStmt{} -> []
    IfElse{} -> []
    Seq ss -> concatMap (genStmt intSize ancInd vmap) ss
  where handleOp var op expr = 
          case op of 
            AddM -> gates ++ add [anc] out varInds ++ reverse gates
            SubM -> gates ++ sub [anc] out varInds ++ reverse gates
            XorM -> gates ++ xor [] out varInds ++ reverse gates
          where (out,gates,anc) = genAExpr' expr
                varInds = fromJust $ lookup var vmap
        genAExpr' = genAExpr intSize ancInd vmap

            
 

genAExpr :: Integer -> Integer -> [(String,[Integer])] -> AExpr -> ([Integer],[Gate],Integer)
genAExpr intSize ancInd vmap expr = (output,gates, head ancFinal)
  where (output, (ancFinal,gates)) = runState (genExpr' expr) ([ancInd..],[])
        genExpr' :: AExpr -> ExprState [Integer]
        genExpr' expr =
          case expr of
            ConstInt n -> mkInt n 
            --Note: fromJust is used, if the var is not in the map it means 
            --an undecleared var is being used an the compiler fails
            --might want to add the possiblity of code gen failing with error
            Var v -> return $ fromJust $ lookup v vmap
            VarInd v _ -> return $ fromJust $ lookup v vmap --TODO !!!
            ABinary op exprA exprB -> applyOp op exprA exprB

        applyOp op opExprA opExprB =
          do a <- genExpr' opExprA
             b <- genExpr' opExprB
             case op of   
               Add  -> applyIPBinOp add a b
               Sub  -> applyIPBinOp sub a b
               Xor  -> applyIPBinOp xor a b
               Mult -> applyOPBinOp mult a b
               _ -> return a

        applyCopy :: [Integer] -> ExprState [Integer]
        applyCopy a = 
          do (anc,gates) <- get
             let copyAnc = take (fromIntegral intSize) anc
             let newAnc = drop (fromIntegral intSize) anc
             put (newAnc, gates ++ copy a copyAnc)
             return copyAnc

        applyIPBinOp :: ([Integer] -> [Integer] -> [Integer] -> [Gate]) -> [Integer] -> [Integer] ->  ExprState [Integer]
        applyIPBinOp op a b  = do a' <- applyCopy a
                                  apply a' b
          where apply a b = 
                  do (anc,gates) <- get
                     put (anc, gates ++ op anc a b)
                     return b

        applyOPBinOp :: ([Integer] -> [Integer] -> [Integer] -> [Integer] -> [Gate]) -> [Integer] -> [Integer] ->  ExprState [Integer]
        applyOPBinOp op a b  = do c <- mkInt 0
                                  apply a b c
          where apply a b c =
                  do (anc,gates) <- get
                     put (anc, gates ++ op anc a b c)
                     return b

        mkInt :: Integer -> ExprState [Integer]
        mkInt n = do (anc,gates) <- get 
                     let ancBits = take (fromIntegral intSize) anc
                     let newAnc =  drop (fromIntegral intSize) anc
                     let newGates = catMaybes $ zipWith (\a b -> if a == 0 then Nothing else Just (Not b)) (bits n) ancBits
                     put (newAnc,gates ++ newGates)
                     return ancBits
          where bits 0 = []
                bits i = mod i 2 : bits (div i 2)

copy :: [Integer] -> [Integer] -> [Gate]
copy (a:as) (b:bs) = Cnot a b : copy as bs
copy [] [] = []

add :: [Integer] -> [Integer] -> [Integer] -> [Gate]
add cs as bs = assert (length as == length bs && not (null as)) $
                 if length as > 1
                 then applyAdd (head cs:as) bs
                 else [Cnot (head as) (head bs)]
  where applyAdd [a0,a1] [b0] = [ Cnot a1 b0
                                , Cnot a0 b0 ]
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ uma a0 b0 a1
        maj x y z = [ Cnot z y
                    , Cnot z x
                    , Toff x y z]
        uma x y z = [ Toff x y z 
                    , Cnot z x
                    , Cnot x y]

-- The extra ignored input is there since this function uses no ancilla
xor :: [Integer] -> [Integer] -> [Integer] -> [Gate]
xor _ = zipWith Cnot

sub :: [Integer] -> [Integer] -> [Integer] -> [Gate]
sub cs as bs = reverse $ add cs as bs

ctrlAdd :: Integer -> Integer -> [Integer] -> [Integer] -> [Gate]
ctrlAdd c ctrl as bs = assert (length as == length bs && not (null as)) $
                        if length as > 1
                        then applyAdd (c:as) bs
                        else [Toff ctrl (head as) (head bs)]
  where applyAdd [a0,a1] [b0] = [ Toff ctrl a1 b0
                                , Toff ctrl a0 b0 ]
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ uma a0 b0 a1
        maj x y z = [ Toff ctrl z y
                    , Cnot z x
                    , Toff x y z]
        uma x y z = [ Toff x y z
                    , Cnot z x
                    , Toff ctrl x y]

mult :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Gate]
mult _ [] _ _ = []
mult cs as bs rs = ctrlAdd c (head as) bs rs ++ mult cs (tail as) (init bs) (tail rs)
  where c = head cs
