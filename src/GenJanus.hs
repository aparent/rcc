module GenJanus (genJanus) where

import ParseJanus
import Circuit

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Data.Maybe

data GenState = GenState { ancInd :: Integer
                         , currGates :: [Gate]
                         } deriving (Show)

data GenConfig = GenConfig { intSize :: Integer
                           , varMap  :: [(String,[Integer])]
                           } deriving (Show)

newtype Gen a =
  Gen { runG :: ReaderT GenConfig (State GenState) a
      } deriving ( Monad, MonadReader GenConfig, MonadState GenState)

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
    pure  = return
    (<*>) = ap

runGen :: Gen a -> Integer -> Integer -> [(String,[Integer])] -> (a, GenState)
runGen k intS ancillaIndex vMap =
    let config = GenConfig { intSize = intS
                           , varMap = vMap
                           }
        state = GenState { ancInd = ancillaIndex
                         , currGates = [] }
    in runState (runReaderT (runG k) config) state

genJanus :: Int -> Janus ->  Circuit
genJanus intSize (decl,stmt) =
  Circuit { circIntSize = intSize
          , inputs = decl
          , gates = mkGates }
  where inputs = zip decl  $ f [0..]
        f ls = take intSize ls : f (drop intSize ls)
        (_,gState) = runGen (genStmt stmt) (fromIntegral intSize) (fromIntegral ancInd) inputs
        mkGates = currGates gState
        ancInd = intSize * length inputs

genStmt :: Stmt -> Gen ()
genStmt stmt =
  do
  vmap <- varMap <$> ask
  intSize <- intSize <$> ask
  currState <- get
  let anc = ancInd currState
  case stmt of
    ModStmt v o e -> addGates $ handleOp vmap anc intSize v o e
    ModIndStmt{} -> return ()
    IfElse cond tStmt eStmt asrt -> genIfElse cond tStmt eStmt asrt
    Seq ss -> mapM_ genStmt ss
  where handleOp vmap ancIndex intSize var op expr =
          case op of
            AddM -> gates ++ add anc out varInds ++ reverse gates
            SubM -> gates ++ sub anc out varInds ++ reverse gates
            XorM -> gates ++ xor anc out varInds ++ reverse gates
          where varInds = fromJust $ lookup var vmap
                (out, gState) = runGen (genAExpr expr) intSize ancIndex vmap
                gates = currGates gState
                anc = ancInd gState


incAncBy :: Integer -> Gen ()
incAncBy n = do currState <- get
                let currAnc = ancInd currState
                put currState { ancInd = currAnc + n }

addGates :: [Gate] -> Gen ()
addGates newGates = do currState <- get
                       let gates = currGates currState
                       put currState { currGates = gates ++ newGates }


genIfElse :: BExpr -> Stmt -> Stmt -> BExpr -> Gen ()
genIfElse condition thenStmt elseStmt assertion =
  do vmap <- varMap <$> ask
     intSize <- intSize <$> ask
     ctrl <- ancInd <$> get
     let swapSize = intSize * fromIntegral (length varsInStmts)
     let swapFrom = let redVmap = filter (\(x,_) -> elem x varsInStmts) vmap
                     in concatMap snd redVmap
     let swapGates = ctrledSwap ctrl swapFrom [ctrl + 1 .. ctrl + swapSize]
     let newVmap = let f ls = take (fromIntegral intSize) ls : f (drop (fromIntegral intSize) ls)
                    in zip varsInStmts $ f [ctrl+1..]
     incAncBy 1
     genBExpr condition ctrl
     incAncBy swapSize
     addGates swapGates
     genStmt thenStmt
     config <- ask
     local (\_->  config {varMap = newVmap}) $ genStmt elseStmt --Else branch uses the other set of gates
     addGates swapGates
     incAncBy $ negate swapSize
     genBExpr assertion ctrl
     incAncBy $ negate 1
  where varsInStmts = varsModInStmt $ Seq [thenStmt,elseStmt]

ctrledSwap :: Integer -> [Integer] -> [Integer] -> [Gate]
ctrledSwap ctrl = zipWith (Fred ctrl)


varsModInStmt :: Stmt -> [String]
varsModInStmt stmt =
 case stmt of
    ModStmt var _ _ -> [var]
    ModIndStmt{} -> []
    IfElse _ s1 s2 _ -> varsModInStmt s1 ++ varsModInStmt s2
    Seq ss -> concatMap varsModInStmt ss

genAExpr :: AExpr -> Gen [Integer]
genAExpr expr = do
  vmap <- varMap <$> ask
  case expr of
    ConstInt n -> mkInt n
    --Note: fromJust is used, if the var is not in the map it means
    --an undecleared var is being used an the compiler fails
    --might want to add the possiblity of code gen failing with error
    Var v -> return $ fromJust $ lookup v vmap
    ABinary op exprA exprB -> applyOp op exprA exprB
  where applyOp op opExprA opExprB =
          do a <- genAExpr opExprA
             b <- genAExpr opExprB
             case op of
               Add  -> applyIPBinOp add a b
               Sub  -> applyIPBinOp sub a b
               Xor  -> applyIPBinOp xor a b
               Mult -> applyOPBinOp mult a b
               _ -> return a

        applyCopy :: [Integer] -> Gen [Integer]
        applyCopy a =
          do intSize <- intSize <$> ask
             anc <- ancInd <$> get
             gates <- currGates <$> get
             let copyAnc = [anc..anc+intSize-1]
             incAncBy intSize
             addGates $ copy a copyAnc
             return copyAnc

        applyIPBinOp :: (Integer -> [Integer] -> [Integer] -> [Gate]) -> [Integer] -> [Integer] ->  Gen [Integer]
        applyIPBinOp op a b  = do b' <- applyCopy b
                                  apply a b'
                                  return b'
          where apply a b =
                  do currState <- get
                     anc <- ancInd <$> get
                     gates <- currGates <$> get
                     addGates $ op anc a b

        applyOPBinOp :: (Integer -> [Integer] -> [Integer] -> [Integer] -> [Gate]) -> [Integer] -> [Integer] ->  Gen [Integer]
        applyOPBinOp op a b  = do c <- mkInt 0
                                  apply a b c
                                  return c
          where apply a b c =
                  do anc <- ancInd <$> get
                     gates <- currGates <$> get
                     addGates $ op anc a b c

        mkInt :: Integer -> Gen [Integer]
        mkInt n = do intSize <- intSize <$> ask
                     anc <- ancInd <$> get
                     gates <- currGates <$> get
                     let ancBits = [anc..anc+intSize-1]
                     let newGates = catMaybes $ zipWith (\a b -> if a == 0 then Nothing else Just (Not b)) (bits n) ancBits
                     addGates newGates
                     incAncBy intSize
                     return ancBits
          where bits 0 = []
                bits i = mod i 2 : bits (div i 2)


genBExpr :: BExpr -> Integer -> Gen Integer
genBExpr expr target =
  case expr of
     RBinary op a b -> do
        outA <- genAExpr a
        outB <- genAExpr b
        case op of
          RLT -> applyROP lessThan target outA outB
  where applyROP :: (Integer -> Integer -> [Integer] -> [Integer] -> [Gate]) ->
                    Integer -> [Integer] -> [Integer] ->  Gen Integer
        applyROP op targ a b =
          do anc <- ancInd <$> get
             addGates $ op anc targ a b
             return targ

copy :: [Integer] -> [Integer] -> [Gate]
copy (a:as) (b:bs) = Cnot a b : copy as bs
copy [] [] = []

add :: Integer -> [Integer] -> [Integer] -> [Gate]
add c as bs = assert (length as == length bs && not (null as)) $
                 if length as > 1
                 then applyAdd (c:as) bs
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
xor :: Integer -> [Integer] -> [Integer] -> [Gate]
xor _ = zipWith Cnot

sub :: Integer -> [Integer] -> [Integer] -> [Gate]
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

lessThan :: Integer -> Integer -> [Integer] -> [Integer] -> [Gate]
lessThan c t as bs = assert (length as == length bs && not (null as)) $
                 if length as > 1
                 then applyAdd (c:as) bs
                 else [Cnot (head as) (head bs)]
  where applyAdd [a0,a1] [b0] = maj a0 b0 a1
                             ++ [Cnot a1 t]
                             ++ reverse (maj a0 b0 a1)
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ reverse (maj a0 b0 a1)
        maj x y z = [ Cnot z y
                    , Cnot z x
                    , Toff x y z]


mult :: Integer -> [Integer] -> [Integer] -> [Integer] -> [Gate]
mult _ [] _ _ = []
mult c as bs rs = ctrlAdd c (head as) bs rs ++ mult c (tail as) (init bs) (tail rs)

--div :: Integer -> [Integer] -> [Integer] -> [Integer] -> [Gate]
--div c

