module GenJanus (genJanus) where

import ParseJanus
import Circuit

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Data.Maybe

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
genJanus intS (decl,stmt) =
  Circuit { circIntSize = intS
          , inputs = decl
          , gates = mkGates }
  where inputMap = zip decl  $ f [0..]
        f ls = take intS ls : f (drop intS ls)
        (_,gState) = runGen (genStmt stmt) intS ancI inputMap
        mkGates = currGates gState
        ancI = intS * length decl

genStmt :: Stmt -> Gen ()
genStmt stmt =
  do
  vmap <- varMap <$> ask
  intS <- intSize <$> ask
  currState <- get
  let anc = ancInd currState
  case stmt of
    ModStmt v o e -> addGates $ handleOp vmap anc intS v o e
    ModIndStmt{} -> return ()
    IfElse cond tStmt eStmt asrt -> genIfElse cond tStmt eStmt asrt
    Seq ss -> mapM_ genStmt ss
  where handleOp vmap ancIndex intS var op expr =
          case op of
            AddM -> gs ++ add anc out varInds ++ reverse gs
            SubM -> gs ++ sub anc out varInds ++ reverse gs
            XorM -> gs ++ xor anc out varInds ++ reverse gs
          where varInds = fromJust $ lookup var vmap
                (out, gState) = runGen (genAExpr expr) intS ancIndex vmap
                gs = currGates gState
                anc = ancInd gState


incAncBy :: Int -> Gen ()
incAncBy n = do currState <- get
                let currAnc = ancInd currState
                put currState { ancInd = currAnc + n }

addGates :: [Gate] -> Gen ()
addGates newGates = do currState <- get
                       let gs = currGates currState
                       put currState { currGates = gs ++ newGates }


genIfElse :: BExpr -> Stmt -> Stmt -> BExpr -> Gen ()
genIfElse condition thenStmt elseStmt assertion =
  do vmap <- varMap <$> ask
     intS <- intSize <$> ask
     ctrl <- ancInd <$> get
     let swapSize = intS * length varsInStmts
     let swapFrom = let redVmap = filter (\(x,_) -> elem x varsInStmts) vmap
                     in concatMap snd redVmap
     let swapGates = ctrledSwap ctrl swapFrom [ctrl + 1 .. ctrl + swapSize]
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
     incAncBy $ negate swapSize
     genBExpr assertion ctrl
     incAncBy $ negate 1
  where varsInStmts = varsModInStmt $ Seq [thenStmt,elseStmt]

ctrledSwap :: Int -> [Int] -> [Int] -> [Gate]
ctrledSwap ctrl = zipWith (Fred ctrl)


varsModInStmt :: Stmt -> [String]
varsModInStmt stmt =
 case stmt of
    ModStmt var _ _ -> [var]
    ModIndStmt{} -> []
    IfElse _ s1 s2 _ -> varsModInStmt s1 ++ varsModInStmt s2
    Seq ss -> concatMap varsModInStmt ss

genAExpr :: AExpr -> Gen [Int]
genAExpr expr = do
  vmap <- varMap <$> ask
  case expr of
    ConstInt n -> mkInt n
    --Note: fromJust is used, if the var is not in the map it means
    --an undecleared var is being used an the compiler fails
    --might want to add the possiblity of code gen failing with error
    Var v -> return $ fromJust $ lookup v vmap
    ABinary op exprA exprB -> applyOp op exprA exprB
    _ -> error $ show expr ++ " is not implemented"
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
                     let newGates = catMaybes $ zipWith (\a b -> if a == 0 then Nothing else Just (Not b)) (bits n) ancBits
                     addGates newGates
                     incAncBy intS
                     return ancBits
          where bits 0 = []
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
          _ -> error  $ show op ++ " is not implemented"
     _ -> error $ show expr ++ " is not implemented"
  where applyROP op targ (a,b) =
          do anc <- ancInd <$> get
             addGates $ op anc targ a b
        mkExprs a b = (,) <$> genAExpr a <*> genAExpr b

copy :: [Int] -> [Int] -> [Gate]
copy = zipWith Cnot

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


mult :: Int -> [Int] -> [Int] -> [Int] -> [Gate]
mult _ [] _ _ = []
mult c as bs rs = ctrlAdd c (head as) bs rs ++ mult c (tail as) (init bs) (tail rs)

--div :: Int -> [Int] -> [Int] -> [Int] -> [Gate]
--div c

