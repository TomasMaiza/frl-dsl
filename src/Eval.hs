module Eval
  ( eval
  , Env
  , noTrace
  , interactive
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
-- Valores de los modos

noTrace :: Mode
noTrace = 0

interactive :: Mode
interactive = 1

-- Entornos
type Env = M.Map Variable List

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

initTrace :: Trace
initTrace = TNil

-- Mónada de estado con manejo de errores y traza
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: Env -> Trace -> Either Error (Pair a (Pair Env Trace)) }

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\env t -> return (x :!: (env :!: t)))
  m >>= f = StateErrorTrace (\env t -> do (x :!: (env' :!: t')) <- runStateErrorTrace m env t
                                          runStateErrorTrace (f x) env' t')

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  track xs = StateErrorTrace (\env t -> return (() :!: (env :!: TCons t xs)))

-- Instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ _ -> Left e)

-- Instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\env t -> lookfor' v env t)
    where lookfor' :: Variable -> Env -> Trace -> Either Error (Pair List (Pair Env Trace))
          lookfor' v' s t = case M.lookup v' s of
                              Nothing -> Left (UndefVar v')
                              Just x -> Right (x :!: (s :!: t))
  update v i = StateErrorTrace (\env t -> return (() :!: (M.insert v i env :!: t)))

-- Evaluador

eval :: Comm -> Mode -> Either Error Trace
eval c m = let env = M.insert "mode" (Unit m) initEnv
           in do (() :!: (_ :!: t)) <- runStateErrorTrace (stepCommStar c) env initTrace
                 return t

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (LetList v ls) = do update v ls
                             return Skip
stepComm (App f ls) = case ls of
                        Var v -> do xs <- lookfor v
                                    stepComm (App f xs)
                        _ -> do xs <- evalFun f ls
                                track $ TApp f ls xs
                                return Skip
stepComm (LetListFun v f ls) = do xs <- evalFun f ls
                                  update v xs
                                  return Skip
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)

evalFun :: (MonadState m, MonadError m, MonadTrace m) => Fun -> List -> m List
evalFun f ls@(Concat _ _) = do zs <- evalConcat ls
                               evalFun f zs
evalFun (Op LeftZero) ls = case ls of
                            Nil -> return (Unit 0)
                            Unit x -> return (Cons 0 Nil x)
                            Cons x zs y -> do cs <- evalConcat (Concat (Unit x) zs)
                                              return (Cons 0 cs y)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftZero) xs
evalFun (Op RightZero) ls = case ls of
                              Nil -> return (Unit 0)
                              Unit x -> return (Cons x Nil 0)
                              Cons x zs y -> do cs <- evalConcat (Concat zs (Unit y))
                                                return (Cons x cs 0)
                              Var v -> do xs <- lookfor v
                                          evalFun (Op RightZero) xs
evalFun (Op LeftDel) ls = case ls of
                            Nil -> throw (DomainErr ls (Op LeftDel))
                            Unit _ -> return Nil
                            Cons _ zs y -> do cs <- evalConcat (Concat zs (Unit y))
                                              return cs
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftDel) xs
evalFun (Op RightDel) ls = case ls of
                            Nil -> throw (DomainErr ls (Op RightDel))
                            Unit _ -> return Nil
                            Cons x zs _ -> do cs <- evalConcat (Concat (Unit x) zs)
                                              return cs
                            Var v -> do xs <- lookfor v
                                        evalFun (Op RightDel) xs
evalFun (Op LeftSucc) ls = case ls of
                            Nil -> throw (DomainErr ls (Op LeftSucc))
                            Unit x -> return (Unit (x + 1))
                            Cons x zs y -> return (Cons (x + 1) zs y)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftSucc) xs
evalFun (Op RightSucc) ls = case ls of
                              Nil -> throw (DomainErr ls (Op RightSucc))
                              Unit x -> return (Unit (x + 1))
                              Cons x zs y -> return (Cons x zs (y + 1))
                              Var v -> do xs <- lookfor v
                                          evalFun (Op RightSucc) xs
evalFun (Repeat f) ls = case ls of
                          Nil -> throw (DomainErr ls (Repeat f))
                          Unit _ -> throw (DomainErr ls (Repeat f))
                          Cons x _ y -> if x == y
                                        then return ls
                                        else do zs <- evalFun f ls
                                                m <- lookfor "mode"
                                                if m == (Unit noTrace) then track TNil else track $ TApp f ls zs
                                                evalFun (Repeat f) zs
                          Var v -> do xs <- lookfor v
                                      evalFun (Repeat f) xs
evalFun (Comp f g) ls = do zs <- evalFun f ls
                           m <- lookfor "mode"
                           if m == (Unit noTrace) then track TNil else track $ TApp f ls zs
                           evalFun g zs
evalFun (Op MoveLeft) ls = evalFun (Comp (Op LeftZero) (Comp (Repeat (Op LeftSucc)) (Op RightDel))) ls
evalFun (Op MoveRight) ls = evalFun (Comp (Op RightZero) (Comp (Repeat (Op RightSucc)) (Op LeftDel))) ls
evalFun (Op DupLeft) ls = evalFun (Comp (Op RightZero) (Comp (Repeat (Op RightSucc)) (Op MoveLeft))) ls
evalFun (Op DupRight) ls = evalFun (Comp (Op LeftZero) (Comp (Repeat (Op LeftSucc)) (Op MoveRight))) ls
evalFun (Op Swap) ls = case ls of
                        Nil -> throw (DomainErr ls (Op Swap))
                        Unit _ -> throw (DomainErr ls (Op Swap))
                        Cons x xs y -> return (Cons y xs x)
                        Var v -> do xs <- lookfor v
                                    evalFun (Op Swap) xs
                                    
evalConcat :: (MonadState m, MonadError m, MonadTrace m) => List -> m List
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
evalConcat (Concat xs@(Concat _ _) ys) = do xs' <- evalConcat xs
                                            evalConcat (Concat xs' ys)
evalConcat (Concat xs ys@(Concat _ _)) = do ys' <- evalConcat ys
                                            evalConcat (Concat xs ys')                                           
evalConcat ls = return ls
