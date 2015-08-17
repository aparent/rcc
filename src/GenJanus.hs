module GenJanus (genJanus) where

import ParseJanus
import Circuit

import Prelude
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
--import Control.Applicative hiding (Const)
import Control.Exception
import Data.Maybe
import Data.List(nub)

data GenState = GenState { ancInd :: Int
                         , currGates :: [Gate]
                         } deriving (Show)

data GenConfig = GenConfig { intSize :: Int
                           , varMap  :: [(String,[Int])]
                           } deriving (Show)

newtype Gen a =
  Gen { runG :: ReaderT GenConfig (State GenState) a
      } deriving ( Monad, MonadReader GenConfig, MonadState GenState)

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
    pure  = return
    (<*>) = ap

runGen :: Gen a -> Int -> Int -> [(String,[Int])] -> (a, GenState)
runGen k intS ancillaIndex vMap =
    let config = GenConfig { intSize = intS
                           , varMap = vMap
                           }
        genState = GenState { ancInd = ancillaIndex
                         , currGates = [] }
    in runState (runReaderT (runG k) config) genState

genJanus :: Int -> Janus ->  Circuit
genJanus intS (Janus (decl,stmt)) =
  Circuit { circIntSize = intS
          , inputs = varNames
          , gates = mkGates }
  where varNames = map fst decl
        inputMap = zip varNames  $ f [0..]
        f ls = take intS ls : f (drop intS ls)
        (_,gState) = runGen (genStmt stmt) intS ancI inputMap
        mkGates = initVars ++ currGates gState
        ancI = intS * length decl
        initVars = concatMap (\ (var,n) -> intGates n (getVar var)) decl
          where getVar v = fromJust $ lookup v inputMap



genStmt :: Stmt -> Gen ()
genStmt stmt =
  do
  vmap <- varMap <$> ask
  intS <- intSize <$> ask
  currState <- get
  let anc = ancInd currState
  case stmt of
    ModStmt v o e -> addGates $ handleOp vmap anc intS v o e
    Loop var start s end ->
      genLoop var start s end
    IfElse cond tStmt eStmt asrt ->
      genIfElse cond tStmt eStmt asrt
    SwapStmt a b -> do
      let indA = fromJust $ lookup a vmap
      let indB = fromJust $ lookup b vmap
      addGates $ zipWith Swap indA indB
    Seq ss ->
      mapM_ genStmt ss
  where handleOp vmap ancIndex intS var op expr =
          case op of
            AddM -> gs ++ add anc out varInds ++ reverse gs
            SubM -> gs ++ sub anc out varInds ++ reverse gs
            XorM -> gs ++ xor anc out varInds ++ reverse gs
          where varInds = fromJust $ lookup var vmap
                (out, gState) = runGen (genAExpr expr) intS ancIndex vmap
                gs = currGates gState
                anc = ancInd gState

genLoop :: String -> Integer -> Stmt -> Integer -> Gen ()
genLoop var start stmt end =
  genStmt $ Seq (map (setVar var stmt) [start .. end])

setVar :: String -> Stmt -> Integer -> Stmt
setVar var stmt value =
  case stmt of
    ModStmt v o e ->
      ModStmt v o (setAE e)
    Loop v start s end ->
      Loop v start (setVar' s) end
    --Can only contain declared variables
    SwapStmt _ _ -> stmt
    IfElse cond tStmt eStmt asrt ->
      IfElse (setBE cond) (setVar' tStmt) (setVar' eStmt) (setBE asrt)
    Seq ss ->
      Seq $ map setVar' ss
  where setVar' s = setVar var s value
        setBE bExpr =
          case bExpr of
            BBinary op exp1 exp2 ->
              BBinary op (setBE exp1) (setBE exp2)
            RBinary op exp1 exp2 ->
              RBinary op (setAE exp1) (setAE exp2)
        setAE aExpr =
          case aExpr of
            Const _ ->
              aExpr
            Var varName
              | varName == var -> Const value
              | otherwise -> aExpr
            ABinary op exp1 exp2 ->
              ABinary op (setAE exp1) (setAE exp2)

incAncBy :: Int -> Gen ()
incAncBy n =
  modify $ \s -> s {ancInd = ancInd s + n}

decAncBy :: Int -> Gen ()
decAncBy n = incAncBy $ negate n

addGates :: [Gate] -> Gen ()
addGates newGates =
  modify $ \s -> s { currGates = currGates s ++ newGates}

-- If statments are done by using a controlled swap to move the bits into the correct branch.
-- The other branch is given the HxH..xH|00..> state.
-- This state is an eigenvector of all permutation matrices so it is unaffected by the reversible block.
-- It will therefore be cleaned up when it is later undone.
-- TODO: Add a citation for this
genIfElse :: BExpr -> Stmt -> Stmt -> BExpr -> Gen ()
genIfElse condition thenStmt elseStmt assertion =
  do vmap <- varMap <$> ask
     intS <- intSize <$> ask
     ctrl <- ancInd <$> get
     let swapSize = intS * length varsInStmts
     let swapFrom = let redVmap = filter (\(x,_) -> elem x varsInStmts) vmap
                     in concatMap snd redVmap
     let swapGates = let swapAnc = [ctrl + 1 .. ctrl + swapSize]
                     in map Hadamard swapAnc ++ ctrledSwap ctrl swapFrom swapAnc
     let newVmap = let f ls = take intS ls : f (drop intS ls)
                    in zip varsInStmts $ f [ctrl+1..]
     incAncBy 1
     genBExpr condition ctrl
     incAncBy swapSize
     addGates swapGates
     genStmt elseStmt
     config <- ask
     local (\_->  config {varMap = newVmap}) $ genStmt thenStmt --Then branch uses the other set of gates
     addGates $ reverse swapGates
     decAncBy swapSize
     genBExpr assertion ctrl
     decAncBy 1
  where varsInStmts = nub $ varsModInStmt $ Seq [thenStmt,elseStmt]

ctrledSwap :: Int -> [Int] -> [Int] -> [Gate]
ctrledSwap ctrl = zipWith (Fred ctrl)


varsModInStmt :: Stmt -> [String]
varsModInStmt stmt =
 case stmt of
    ModStmt var _ _ -> [var]
    IfElse _ s1 s2 _ -> varsModInStmt s1 ++ varsModInStmt s2
    Loop _ _ s _ -> varsModInStmt s
    SwapStmt a b -> [a,b]
    Seq ss -> concatMap varsModInStmt ss

genAExpr :: AExpr -> Gen [Int]
genAExpr expr = do
  vmap <- varMap <$> ask
  case expr of
    Const n -> mkInt n
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

        applyCopy :: [Int] -> Gen [Int]
        applyCopy a =
          do intS <- intSize <$> ask
             anc <- ancInd <$> get
             let copyAnc = [anc..anc+intS-1]
             incAncBy intS
             addGates $ copy a copyAnc
             return copyAnc

        applyIPBinOp :: (Int -> [Int] -> [Int] -> [Gate]) -> [Int] -> [Int] ->  Gen [Int]
        applyIPBinOp op inpA inpB  = do inpB' <- applyCopy inpB
                                        apply inpA inpB'
                                        return inpB'
          where apply a b =
                  do anc <- ancInd <$> get
                     addGates $ op anc a b

        applyOPBinOp :: (Int -> [Int] -> [Int] -> [Int] -> [Gate]) -> [Int] -> [Int] ->  Gen [Int]
        applyOPBinOp op inpA inpB  = do c <- mkInt 0
                                        apply inpA inpB c
                                        return c
          where apply a b c =
                  do anc <- ancInd <$> get
                     addGates $ op anc a b c

        mkInt :: Integer -> Gen [Int]
        mkInt n = do intS <- intSize <$> ask
                     anc <- ancInd <$> get
                     let ancBits = [anc..anc+intS-1]
                     let newGates = intGates n ancBits
                     addGates newGates
                     incAncBy intS
                     return ancBits

intGates :: Integer -> [Int] -> [Gate]
intGates n targs = catMaybes $ zipWith bitToGate (bits n) targs
          where bitToGate bit targ =
                  if bit == 0 then
                    Nothing
                  else
                    Just (Not targ)
                bits 0 = []
                bits i = mod i 2 : bits (div i 2)

aExprWithCleanup :: Gen a -> (a -> Gen b) -> Gen b
aExprWithCleanup expr op =
  do ancBefore <- ancInd <$> get
     gateAmtBefore <- length . currGates <$> get
     x <- expr
     exprGates <- drop gateAmtBefore . currGates <$> get
     ancAfter <- ancInd <$> get
     ret <- op x
     addGates $ reverse exprGates
     incAncBy (ancBefore - ancAfter)
     return ret

genBExpr :: BExpr -> Int -> Gen ()
genBExpr expr target =
  case expr of
     RBinary op a b ->
        case op of
          RLT -> aExprWithCleanup (mkExprs a b) (applyROP lessThan target)
          RGT -> aExprWithCleanup (mkExprs a b) (applyROP greaterThan target)
          _ -> error  $ show op ++ " is not implemented"
     _ -> error $ show expr ++ " is not implemented"
  where applyROP op targ (a,b) =
          do anc <- ancInd <$> get
             addGates $ op anc targ a b
        mkExprs a b = (,) <$> genAExpr a <*> genAExpr b

copy :: [Int] -> [Int] -> [Gate]
copy = zipWith Cnot

-- Creates an addition circuit based on http://arxiv.org/abs/quant-ph/0410184
add :: Int -> [Int] -> [Int] -> [Gate]
add c inpA inpB = assert (length inpA == length inpB && not (null inpA)) $
                 if length inpA > 1
                 then applyAdd (c:inpA) inpB
                 else [Cnot (head inpA) (head inpB)]
  where applyAdd [a0,a1] [b0] = [ Cnot a1 b0
                                , Cnot a0 b0 ]
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ uma a0 b0 a1
        applyAdd _ _ = error "Should be impossible by equal length assertion."
        maj x y z = [ Cnot z y
                    , Cnot z x
                    , Toff x y z]
        uma x y z = [ Toff x y z
                    , Cnot z x
                    , Cnot x y]

-- The extra ignored input is there since this function uses no ancilla
xor :: Int -> [Int] -> [Int] -> [Gate]
xor _ = zipWith Cnot

sub :: Int -> [Int] -> [Int] -> [Gate]
sub cs as bs = reverse $ add cs as bs

--   Creates a controlled addition circuit.
--   Uses the addition circuit from: http://arxiv.org/abs/quant-ph/0410184
--   Controlled by adding controls to a set of gates such that removing that set
--   Would cause the circuit to be the identity
ctrlAdd :: Int -> Int -> [Int] -> [Int] -> [Gate]
ctrlAdd c ctrl inpA inpB = assert (length inpA == length inpB && not (null inpA)) $
                        if length inpA > 1
                      then applyAdd (c:inpA) inpB
                      else [Toff ctrl (head inpA) (head inpB)]
  where applyAdd [a0,a1] [b0] = [ Toff ctrl a1 b0
                                , Toff ctrl a0 b0 ]
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ uma a0 b0 a1
        applyAdd _ _ = error "Should be impossible by equal length assertion."
        maj x y z = [ Toff ctrl z y
                    , Cnot z x
                    , Toff x y z]
        uma x y z = [ Toff x y z
                    , Cnot z x
                    , Toff ctrl x y]

-- Creates a comparison circuit based on http://arxiv.org/abs/quant-ph/0410184
lessThan :: Int -> Int -> [Int] -> [Int] -> [Gate]
lessThan c t inpA inpB =
  assert (length inpA == length inpB && not (null inpB)) $
  notA ++ highBitAdder ++ notA
  where notA = map Not inpA
        highBitAdder =
          if length inpA > 1
          then applyAdd (c:inpA) inpB
          else [Cnot (head inpA) (head inpB)]
        applyAdd [a0,a1] [b0] = maj a0 b0 a1
                             ++ [Cnot a1 t]
                             ++ reverse (maj a0 b0 a1)
        applyAdd (a0:a1:as) (b0:bs)  = maj a0 b0 a1
                                    ++ applyAdd (a1:as) bs
                                    ++ reverse (maj a0 b0 a1)
        applyAdd _ _ = error "Should be impossible by equal length assertion."
        maj x y z = [ Cnot z y
                    , Cnot z x
                    , Toff x y z]

greaterThan :: Int -> Int -> [Int] -> [Int] -> [Gate]
greaterThan c t a b = lessThan c t b a

-- Controlled addition based adder.
-- Uses the simple shift and add method
mult :: Int -> [Int] -> [Int] -> [Int] -> [Gate]
mult _ [] _ _ = []
mult c as bs rs = ctrlAdd c (head as) bs rs ++ mult c (tail as) (init bs) (tail rs)

divide :: Int -> [Int] -> [Int] -> [Int] -> [Gate]
divide as bs rs = undefined
