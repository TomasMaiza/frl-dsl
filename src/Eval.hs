module Eval
  ( evalFun
  , eval
  , Env
  , initEnv
  , runStateErrorTrace
  --, eval'
  -- borrar todo y dejar solo eval y Env
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

initTrace :: Trace
initTrace = ""

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
  track str = StateErrorTrace (\env t -> return (() :!: (env :!: t ++ str)))

-- Instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ _ -> Left e)

-- Instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\env t -> lookfor' v env t)
    where lookfor' :: Variable -> Env -> Trace -> Either Error (Pair List (Pair Env Trace))
          lookfor' v' s t = case M.lookup v' s of
                              Nothing -> Left UndefVar
                              Just x -> Right (x :!: (s :!: t))
  update v i = StateErrorTrace (\env t -> return (() :!: (M.insert v i env :!: t)))

{-
eval' :: Comm -> Either Error List
eval' c = do (xs :!: _) <- runStateErrorTrace (stepCommStar c) initEnv initTrace
             return xs-}

{-
eval :: Fun -> List -> Either Error (Pair Env Trace)
eval f ls = do (_ :!: (env :!: t)) <- runStateErrorTrace (evalFun f ls) initEnv initTrace
               return (env :!: t)

eval' :: Fun -> List -> Either Error List
eval' f ls = do (xs :!: _) <- runStateErrorTrace (evalFun f ls) initEnv initTrace
                return xs-}

eval :: Comm -> Either Error (Pair Env Trace)
eval c = do (() :!: (env :!: t)) <- runStateErrorTrace (stepCommStar c) initEnv initTrace
            return (env :!: t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (LetList v ls) = do update v ls
                             track $ v ++ " = " ++ show ls ++ "; "
                             return Skip
stepComm (App f ls) = do xs <- evalFun f ls -- otra opción es trackear "f ls = xs"
                         track $ show f ++ " " ++ show ls ++ " = " ++ show xs ++ "; "
                         return Skip
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)

evalFun :: (MonadState m, MonadError m, MonadTrace m) => Fun -> List -> m List
evalFun (Op LeftZero) ls = case ls of
                            Nil -> return (Unit 0)
                            Unit x -> return (Cons 0 Nil x)
                            Cons x zs y -> return (Cons 0 (Concat (Unit x) zs) y)
                            Concat xs ys -> do zs <- evalFun (Op LeftZero) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftZero) xs
evalFun (Op RightZero) ls = case ls of
                              Nil -> return (Unit 0)
                              Unit x -> return (Cons x Nil 0)
                              Cons x zs y -> return (Cons x (Concat zs (Unit y)) 0)
                              Concat xs ys -> do zs <- evalFun (Op RightZero) ys
                                                 return (Concat xs zs)
                              Var v -> do xs <- lookfor v
                                          evalFun (Op RightZero) xs
evalFun (Op LeftDel) ls = case ls of
                            Nil -> throw DomainErr
                            Unit _ -> return Nil
                            Cons _ zs y -> return (Concat zs (Unit y))
                            Concat xs ys -> do zs <- evalFun (Op LeftDel) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftDel) xs
evalFun (Op RightDel) ls = case ls of
                            Nil -> throw DomainErr
                            Unit _ -> return Nil
                            Cons x zs _ -> return (Concat (Unit x) zs)
                            Concat xs ys -> do zs <- evalFun (Op RightDel) ys
                                               return (Concat xs zs)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op RightDel) xs
evalFun (Op LeftSucc) ls = case ls of
                            Nil -> throw DomainErr
                            Unit x -> return (Unit (x + 1))
                            Cons x zs y -> return (Cons (x + 1) zs y)
                            Concat xs ys -> do zs <- evalFun (Op LeftSucc) xs
                                               return (Concat zs ys)
                            Var v -> do xs <- lookfor v
                                        evalFun (Op LeftSucc) xs
evalFun (Op RightSucc) ls = case ls of
                              Nil -> throw DomainErr
                              Unit x -> return (Unit (x + 1))
                              Cons x zs y -> return (Cons x zs (y + 1))
                              Concat xs ys -> do zs <- evalFun (Op RightSucc) ys
                                                 return (Concat xs zs)
                              Var v -> do xs <- lookfor v
                                          --track (show v ++ " = " ++ show xs ++ "; ")
                                          evalFun (Op RightSucc) xs
evalFun (Repeat f) ls = case ls of
                          Nil -> throw DomainErr
                          Unit _ -> throw DomainErr
                          Cons x _ y -> if x == y
                                        then return ls
                                        else do zs <- evalFun f ls
                                                track $ show zs ++ "; "
                                                evalFun (Repeat f) zs
                          Concat _ _ -> do zs <- evalConcat ls
                                           evalFun (Repeat f) zs
                          Var v -> do xs <- lookfor v
                                      --track (show v ++ " = " ++ show xs ++ "; ")
                                      evalFun (Repeat f) xs
evalFun (Comp f g) ls = do zs <- evalFun f ls
                           track $ show zs ++ "; "
                           evalFun g zs
evalFun (Op MoveLeft) ls = evalFun (Comp (Op LeftZero) (Comp (Repeat (Op LeftSucc)) (Op RightDel))) ls
evalFun (Op MoveRight) ls = evalFun (Comp (Op RightZero) (Comp (Repeat (Op RightSucc)) (Op LeftDel))) ls
evalFun (Op DupLeft) ls = evalFun (Comp (Op RightZero) (Comp (Repeat (Op RightSucc)) (Op MoveLeft))) ls
evalFun (Op DupRight) ls = evalFun (Comp (Op LeftZero) (Comp (Repeat (Op LeftSucc)) (Op MoveRight))) ls
evalFun (Op Swap) ls = case ls of
                        Nil -> throw DomainErr
                        Unit _ -> throw DomainErr
                        Cons x xs y -> return (Cons y xs x)
                        Concat _ _ -> do zs <- evalConcat ls
                                         evalFun (Op Swap) zs
                        Var v -> do xs <- lookfor v
                                    evalFun (Op Swap) xs
{-
evalFun (Op Swap) ls = let r = Repeat (Comp (Op LeftSucc) (Comp (Op MoveRight) (Comp (Op MoveRight) (Comp (Op LeftSucc) (Comp (Op MoveLeft) (Op MoveLeft))))))
                       in evalFun (Comp (Op MoveRight) (Comp (Op LeftZero) (Comp (Op MoveLeft) (Comp r (Comp (Op RightDel) (Comp (Op LeftDel) (Op MoveRight))))))) ls
-}

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
evalConcat ls = return ls