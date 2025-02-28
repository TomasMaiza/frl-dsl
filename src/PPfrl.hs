module PPfrl where

import           AST
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

tabW :: Int
tabW = 2

pVar :: Variable -> Doc
pVar = text

{-
(Cons 0 (Cons 3 Nil 1) 3)
[0, (Cons 3 Nil 1), 3]
[0,  ] -}

pList :: List -> Doc
pList Nil = text "[]"
pList (Unit x) = brackets $ int x
pList (Cons x Nil y) = brackets (int x <> text "," <+> int y)
pList (Cons x ls y) = brackets (int x <> text "," <+> pListRec ls <> text "," <+> int y)
pList (Concat xs Nil) = pList xs
pList (Concat Nil ys) = pList ys
pList (Concat xs ys) = brackets (pListRec xs <> text "," <+> pListRec ys)
pList (Var v) = pVar v

pListRec :: List -> Doc -- imprime listas sin brackets para los casos recursivos
pListRec Nil = empty
pListRec (Unit x) = int x
pListRec (Cons x Nil y) = int x <> text "," <+> int y
pListRec (Cons x ls y) = int x <> text "," <+> pListRec ls <> text "," <+> int y
pListRec (Concat xs Nil) = pListRec xs
pListRec (Concat Nil ys) = pListRec ys
pListRec (Concat xs ys) = pListRec xs <> text "," <+> pListRec ys
pListRec (Var v) = pVar v

pFun :: Fun -> Doc
pFun (Op LeftZero) = text "0i"
pFun (Op RightZero) = text "0d"
pFun (Op LeftDel) = text "Bi"
pFun (Op RightDel) = text "Bd"
pFun (Op LeftSucc) = text "Si"
pFun (Op RightSucc) = text "Sd"
pFun (Op MoveLeft) = text "<-"
pFun (Op MoveRight) = text "->"
pFun (Op DupLeft) = text "Dd"
pFun (Op DupRight) = text "Di"
pFun (Op Swap) = text "<->"
pFun (Repeat f) = text "<" <> pFun f <> text ">"
pFun (Comp f g) = pFun f <+> pFun g

pComm :: Comm -> Doc
pComm Skip        = empty
pComm (LetList v ls) = pVar v <+> text "=" <+> pList ls
pComm (LetListFun v f ls) = pVar v <+> text "=" <+> pFun f <+> pList ls
pComm (Seq c1 c2) = pComm c1 <> semi $$ pComm c2
pComm (App f ls) = pFun f <+> pList ls

pError :: Error -> Doc
pError = undefined -- hace falta?

pTrace :: Trace -> Doc
pTrace (TLetList v ls) = pVar v <+> text "=" <+> pList ls $$ empty
pTrace (TApp f ls zs) = pFun f <+> pList ls <+> text "=" <+> pList zs
pTrace (TList ls) = pList ls
pTrace (TCons t1 t2) = pTrace t1 $$ pTrace t2
pTrace TNil = empty

renderComm :: Comm -> String
renderComm = render . pComm

renderFun :: Fun -> String
renderFun = render . pFun

renderList :: List -> String
renderList = render . pList

renderTrace :: Trace -> String
renderTrace = render . pTrace
