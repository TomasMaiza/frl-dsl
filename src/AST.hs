module AST where

-- Identificadores de Variable
type Variable = String
type Nat = Int

-- Listas
data List = Nil 
          | Unit Nat
          | Cons Nat List Nat
          | Concat List List
          | Var Variable
          deriving (Show, Eq)

-- Operaciones
data Op
  = LeftZero
  | RightZero
  | LeftDel
  | RightDel
  | LeftSucc
  | RightSucc
  | MoveLeft
  | MoveRight
  | DupLeft
  | DupRight
  | Swap
  deriving (Show, Eq)

-- Funciones
data Fun
  = Op Op
  | Repeat Fun
  | Comp Fun Fun
  | FunVar Variable
  deriving (Show, Eq)

-- Comandos
data Comm
  = App Fun List
  | LetList Variable List
  | LetListFun Variable Fun List
  | LetFun Variable Fun
  | Seq Comm Comm
  | Eq Fun List List -- solo compara entradas de la forma "f xs == ys"
  | NEq Fun List List
  | Print Variable
  | Skip
  deriving (Show, Eq)

data Value = VList List | VFun Fun | VMode Mode deriving (Show, Eq)

data Error = DomainErr List Fun | UndefVar Variable | VarError Variable Value deriving (Eq, Show)

data Trace
  = TPrintList Variable List
  | TPrintFun Variable Fun
  | TApp Fun List List
  | TList List
  | TCons Trace Trace
  | TNil
  | TTrue
  | TFalse
  deriving (Show, Eq)

type Mode = Nat -- en el caso interactivo se ve toda la traza de la aplicación de una función
                -- es decir el paso a paso de cada función compuesta
                -- si es un archivo, solo muestra los resultados



{-
Fun -> Fun Fun | Id
-------------------
Fun -> Id Fun'
Fun' -> ε | Fun Fun'
-}