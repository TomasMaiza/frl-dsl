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
  deriving (Show, Eq)

data Error = DomainErr | UndefVar deriving (Eq, Show)

type Trace = String

{-
Fun -> Fun Fun | Id
-------------------
Fun -> Id Fun'
Fun' -> ε | Fun Fun'
-}