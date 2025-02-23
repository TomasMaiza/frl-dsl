module Eval
  ( evalFun
  , eval
  , Env
  , initEnv
  , runState
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable List

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s
                         in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
{-stepFun:: MonadState m => Fun -> m ()
stepFun Skip = return ()
stepFun c    = stepComm c >>= \c' -> stepCommStar c'-}

-- Evalua un programa en el estado nulo
{-eval :: Fun -> Env
eval p = snd (runState p initEnv)-}

eval :: Fun -> List -> Env
eval f ls = snd $ runState (evalFun f ls) initEnv

evalFun :: MonadState m => Fun -> List -> m List
evalFun (Op LeftZero) ls = case ls of
                            Nil -> return (Unit 0)
                            Unit x -> return (Cons 0 Nil x)
                            Cons x ls y -> return (Cons 0 (Concat (Unit x) ls) y)
                            Concat xs ys -> do zs <- evalFun (Op LeftZero) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftZero) xs
evalFun (Op RightZero) ls = case ls of
                              Nil -> return (Unit 0)
                              Unit x -> return (Cons x Nil 0)
                              Cons x ls y -> return (Cons x (Concat ls (Unit y)) 0)
                              Concat xs ys -> do zs <- evalFun (Op RightZero) ys
                                                 return (Concat xs zs)
                              Var v -> do xs <- lookfor v
                                          evalFun (Op RightZero) xs
evalFun (Op LeftDel) ls = case ls of
                            --Nil -> error
                            Unit x -> return Nil
                            Cons x ls y -> return (Concat ls (Unit y))
                            Concat xs ys -> do zs <- evalFun (Op LeftDel) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftDel) xs
evalFun (Op RightDel) ls = case ls of
                            --Nil -> error
                            Unit x -> return Nil
                            Cons x ls y -> return (Concat (Unit x) ls)
                            Concat xs ys -> do zs <- evalFun (Op RightDel) ys
                                               return (Concat xs zs)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op RightDel) xs
evalFun (Op LeftSucc) ls = case ls of
                            --Nil -> error
                            Unit x -> return (Unit (x + 1))
                            Cons x ls y -> return (Cons (x + 1) ls y)
                            Concat xs ys -> do zs <- evalFun (Op LeftSucc) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftSucc) xs
evalFun (Op RightSucc) ls = case ls of
                              --Nil -> error
                              Unit x -> return (Unit (x + 1))
                              Cons x ls y -> return (Cons x ls (y + 1))
                              Concat xs ys -> do zs <- evalFun (Op RightSucc) ys
                                                 return (Concat xs zs)
                              Var v -> do xs <- lookfor v
                                          evalFun (Op RightSucc) xs
evalFun (Repeat f) ls = case ls of
                          --Nil -> error
                          --Unit _ -> error
                          Cons x zs y -> if x == y
                                         then return ls
                                         else do zs <- evalFun f ls
                                                 evalFun (Repeat f) zs
                          Concat xs ys -> do zs <- evalConcat ls
                                             evalFun (Repeat f) zs
                          Var v -> do xs <- lookfor v
                                      evalFun (Repeat f) xs
evalFun (Comp f g) ls = do zs <- evalFun (Op f) ls
                           evalFun g zs
{-
evalFun (MoveLeft ls) = case ls of
                          --Nil -> error
                          Unit x -> return (Unit x)
                          Cons x zs y -> Concat (Unit y) (Concat (Unit x) zs)

-}

evalConcat :: MonadState m => List -> m List
evalConcat (Concat Nil ys) = return ys 
evalConcat (Concat xs Nil) = return xs
evalConcat (Concat (Unit x) (Unit y)) = return (Cons x Nil y)
evalConcat (Concat (Unit x) (Cons x' ls y')) = do zs <- evalConcat (Concat (Unit x') ls)
                                                  return (Cons x zs y')
evalConcat (Concat (Cons x ls y) (Unit z)) = do zs <- evalConcat (Concat ls (Unit y))
                                                return (Cons x zs z)
evalConcat (Concat (Cons x ls y) (Cons x' ls' y')) = do as <- evalConcat (Concat ls (Unit y))
                                                        bs <- evalConcat (Concat (Unit x') ls')
                                                        zs <- evalConcat (Concat as bs)
                                                        return (Cons x zs y') 