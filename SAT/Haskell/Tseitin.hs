module Tseitin(
  Solution(..),
  satisfiable
) where

import           Control.Monad.State
import           Data.Hashable
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Formula
import qualified Picosat
import System.Mem.StableName       
       
type Literal = Int
type Clause = [Literal]

data Solution a
  = Unsatisfiable
  | Satisfiable (Map a Bool)
    deriving Show

-- state
data St a
  = St {
    names         :: [Int]
  , ids           :: Map a Int
  , vars          :: Map Int a
  , tseitinMemo   :: HashMap (StableName (Formula a)) Int
  , clauses       :: [Clause]
  }

type MT a m = StateT (St a) m

runWithSt :: (MonadIO m, Ord a) => MT a m b -> m b
runWithSt m = evalStateT m initSt
  where initSt = St {
            names = [1..]
          , ids = Map.empty
          , vars = Map.empty
          , tseitinMemo = HashMap.empty
          , clauses = []
          }

-- Return a fresh variable id.
freshId :: MonadIO m => MT a m Int
freshId = do
  x:xs <- gets names
  modify $ \s -> s { names = xs }
  return x

-- Add clause c to the list of produced clauses.
emit :: Monad m => Clause -> MT a m ()
emit c = modify (\st -> st { clauses = c:(clauses st) })

-- Get the id of variable v. If v does not have an id, associate a fresh one.
getId :: (Ord a, Eq a, Monad m) => a -> MT a m Int
getId v = do
  st <- get
  let x:xs = names st
  case Map.lookup v (ids st) of
    Just i -> return i
    Nothing -> do
      put $ st {
        names = xs,
        ids = Map.insert v x (ids st),
        vars = Map.insert x v (vars st)
        }
      return x

-- Given a solution, return a variable assignment.
solution :: (Ord a, MonadIO m) => Picosat.Solution -> MT a m (Solution a)
solution Picosat.Unknown = error "picosat: unknown result"
solution Picosat.Unsatisfiable = return Unsatisfiable
solution (Picosat.Solution sol) = do
  st <- get
  let setvar x m =
        let (i, b) = if x >= 0 then (x, True) else (-x, False) in
        case Map.lookup i (vars st) of
          Nothing -> m
          Just v -> Map.insert v b m
      eta = foldr setvar Map.empty sol
  return $ Satisfiable eta

-- Tseitin transformation.
tseitin :: (MonadIO m, Ord a) => Formula a -> MT a m Int

tseitin f = do
  memo <- gets tseitinMemo
  n <- liftIO $ makeStableName f
  case HashMap.lookup n memo of
    Nothing -> do
      x <- tseitin' f
      n <- liftIO $ makeStableName f
      modify $ \s -> s { tseitinMemo = HashMap.insert n x (tseitinMemo s) }
      return x
    Just x -> return x
  
tseitin' :: (MonadIO m, Ord a) => Formula a -> MT a m Int
  
tseitin' (Var v) =
  getId v
    
tseitin' TT = do
  x <- freshId
  emit [x]
  return x
  
tseitin' FF = do
  x <- freshId
  emit [-x]
  return x

tseitin' (Neg phi1) = do
  x1 <- tseitin phi1
  x <- freshId
  emit [x, x1]
  emit [-x, -x1]
  return x

tseitin' (Or phi1 phi2) = do
  x1 <- tseitin phi1
  x2 <- tseitin phi2
  x <- freshId
  emit [-x, x1, x2]
  emit [-x1, x]
  emit [-x2, x]
  return x

tseitin' (And phi1 phi2) = do
  x1 <- tseitin phi1
  x2 <- tseitin phi2
  x <- freshId
  emit [-x, x1]
  emit [-x, x2]
  emit [x, -x1, -x2]
  return x

tseitin' (Xor phi1 phi2) = do
  x1 <- tseitin phi1
  x2 <- tseitin phi2
  x <- freshId
  emit [-x, x1, x2]
  emit [-x, -x1, -x2]
  emit [x, x1, -x2]
  emit [x, -x1, x2]
  return x
  
tseitin' (Imp phi1 phi2) = do
  x1 <- tseitin phi1
  x2 <- tseitin phi2
  x <- freshId
  emit [-x, -x1, x2]
  emit [x, x1]
  emit [x, -x2]
  return x
  
tseitin' (Iff phi1 phi2) = do
  x1 <- tseitin phi1
  x2 <- tseitin phi2
  x <- freshId
  emit [x, x1, x2]
  emit [x, -x1, -x2]
  emit [-x, x1, -x2]
  emit [-x, -x1, x2]
  return x

tseitin' (BigOr phis) = do
  xs <- mapM tseitin phis
  x <- freshId
  _ <- forM xs (\xi -> emit [x, -xi])
  emit (-x : xs)
  return x

tseitin' (BigAnd phis) = do
  xs <- mapM tseitin phis
  x <- freshId
  _ <- forM xs (\xi -> emit [-x, xi])
  emit (x : [-xi | xi <- xs])
  return x

-- Check phi for satisfiablity. If true, return a satisfying assignment. in the
satisfiable :: (Ord a, Eq a, Show a) => Formula a -> IO (Solution a)
satisfiable phi = runWithSt $ do
  x <- tseitin phi
  cs <- gets clauses
  eta <- liftIO $ Picosat.solve ([x] : cs)
  solution eta
