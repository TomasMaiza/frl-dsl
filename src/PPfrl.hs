module PPfrl where

import           AST
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

tabW :: Int
tabW = 2

pVar :: Variable -> Doc
pVar = text

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
pFun (Op MoveLeft) = text "(<-)"
pFun (Op MoveRight) = text "(->)"
pFun (Op DupLeft) = text "Di"
pFun (Op DupRight) = text "Dd"
pFun (Op Swap) = text "(<->)"
pFun (Repeat f) = text "<" <> pFun f <> text ">"
pFun (Comp f g) = pFun f <+> pFun g
pFun (FunVar v) = pVar v

pComm :: Comm -> Doc
pComm Skip        = empty
pComm (LetList v ls) = pVar v <+> text "=" <+> pList ls
pComm (LetListFun v f ls) = pVar v <+> text "=" <+> pFun f <+> pList ls
pComm (Seq c1 c2) = pComm c1 <> semi $$ pComm c2
pComm (App f ls) = pFun f <+> pList ls
pComm (LetFun v f) = pVar v <+> text "=" <+> pFun f
pComm (Eq f xs ys) = pFun f <+> pList xs <+> text "==" <+> pList ys
pComm (NEq f xs ys) = pFun f <+> pList xs <+> text "!=" <+> pList ys
pComm (Print v) = text "print" <+> pVar v
pComm (GenApp f xs _) = pFun f <+> pGenList xs

pError :: Error -> Doc
pError (UndefVar v) = text "Runtime error: variable" <+> doubleQuotes (pVar v) <+> text "undefined"
pError (DomainErr ls f) = text "Runtime error: domain error at" <+> pComm (App f ls)
pError (VarError v (VList _)) = text "Runtime error: expected function but" <+> doubleQuotes (pVar v) <+> text "is a list" 
pError (VarError v (VFun _)) = text "Runtime error: expected list but" <+> doubleQuotes (pVar v) <+> text "is a function" 
pError (VarError _ (VMode _)) = text "Runtime error: trying to print mode"
pError (GenListErr ls f) = text "Runtime error: generic list error at" <+> pComm (GenApp f ls 1)

pTrace :: Trace -> Doc
pTrace (TPrintList v ls) = pVar v <+> text "=" <+> pList ls $$ empty
pTrace (TPrintFun v f) = pVar v <+> text "=" <+> pFun f $$ empty
pTrace (TApp f ls zs) = pFun f <+> pList ls <+> text "=" <+> pList zs
pTrace (TList ls) = pList ls
pTrace (TCons t1 t2) = pTrace t1 $$ pTrace t2
pTrace TNil = empty
pTrace TTrue = text "True"
pTrace TFalse = text "False"
pTrace (TGenApp f _ ys) = pFun f <+> text "|" <+> pGenList ys -- intento de hacer la tabla

pGenElem :: GenElem -> Doc
pGenElem GNull = text ""
pGenElem (GElem v) = pVar v
pGenElem (GSucc e) = let (v, n) = calcSuc e 1
                     in pVar v <+> text "+" <+> int n
pGenElem (GNat n) = int n

calcSuc :: GenElem -> Int -> (Variable, Int)
calcSuc (GElem v) n = (v, n)
calcSuc (GSucc e) n = calcSuc e (n + 1)

pGenList :: GenList -> Doc
pGenList GNil = text "[]"
pGenList (GList v) = brackets $ pVar v
pGenList (GCons GNull ls GNull) = pGenList ls
pGenList (GCons GNull GNil y) = brackets $ pGenElem y
pGenList (GCons x GNil GNull) = brackets $ pGenElem x
pGenList (GCons x GNil y) = brackets $ (pGenElem x <> text "," <+> pGenElem y)
pGenList (GCons GNull ls y) = brackets $ (pGenListRec ls <> text "," <+> pGenElem y)
pGenList (GCons x ls GNull) = brackets $ (pGenElem x <> text "," <+> pGenListRec ls)
pGenList (GCons x ls y) = brackets $ (pGenElem x <> text "," <+> pGenListRec ls <> text "," <+> pGenElem y)
pGenList (GConcat xs ys) = brackets $ (pGenListRec xs <> text "," <+> pGenListRec ys)
pGenList (GUnit x) = brackets $ pGenElem x

pGenListRec :: GenList -> Doc
pGenListRec GNil = empty
pGenListRec (GList v) = pVar v
pGenListRec (GCons GNull ls GNull) = pGenListRec ls
pGenListRec (GCons GNull GNil y) = pGenElem y
pGenListRec (GCons x GNil GNull) = pGenElem x
pGenListRec (GCons x GNil y) = pGenElem x <> text "," <+> pGenElem y
pGenListRec (GCons GNull ls y) = pGenListRec ls <> text "," <+> pGenElem y
pGenListRec (GCons x ls GNull) = pGenElem x <> text "," <+> pGenListRec ls
pGenListRec (GCons x ls y) = pGenElem x <> text "," <+> pGenListRec ls <> text "," <+> pGenElem y
pGenListRec (GConcat xs ys) = pGenListRec xs <> text "," <+> pGenListRec ys
pGenListRec (GUnit x) = pGenElem x

renderComm :: Comm -> String
renderComm = render . pComm

renderFun :: Fun -> String
renderFun = render . pFun

renderList :: List -> String
renderList = render . pList

renderTrace :: Trace -> String
renderTrace = render . pTrace

renderError :: Error -> String
renderError = render . pError

renderGenElem :: GenElem -> String
renderGenElem = render . pGenElem

renderGenList :: GenList -> String
renderGenList = render. pGenList