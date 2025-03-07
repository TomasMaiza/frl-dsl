module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import           Data.Char
import           Control.Monad (guard)

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace frl
  t <- p
  eof
  return t

-- Analizador de Tokens
frl :: TokenParser u
frl = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["mode"
                        , "["
                        , "]"
                        , "<"
                        , ">"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "0i"
                        , "0d"
                        , "Si"
                        , "Sd"
                        , "Bi"
                        , "Bd"
                        , "(<->)"
                        , "Di"
                        , "Dd"
                        , "(->)"
                        , "(<-)"
                        , "{"
                        , "}"
                        , "print"
                        , "stop"]
    , reservedOpNames = [ "["
                        , "]"
                        , "<"
                        , ">"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "0i"
                        , "0d"
                        , "Si"
                        , "Sd"
                        , "Bi"
                        , "Bd"
                        , "(<->)"
                        , "Di"
                        , "Dd"
                        , "(->)"
                        , "(<-)"
                        , "{"
                        , "}"
                        ]
    }
  )

-- Parser de listas

list :: Parser List
list = do parseVarList
       <|> brackets frl (try parseElems
                         <|> return Nil) 

parseNat :: Parser List
parseNat = do x <- natural frl
              return $ Unit $ fromIntegral x

parseElems :: Parser List
parseElems = chainl1 (do {parseNat <|> parseVarList}) (do {reservedOp frl ","; return Concat})

parseVarList :: Parser List
parseVarList = do v <- identifier frl
                  guard (length v == 1 && isUpper (head v))
                  return (Var v)


-- Parser de funciones

fun :: Parser Fun
fun = do parseVarFun <|> chainr1 parseOp (return Comp)

parseVarFun :: Parser Fun
parseVarFun = do v <- identifier frl
                 guard (length v > 1 && isLower (head v))
                 return (FunVar v)

parseOp :: Parser Fun
parseOp = (do {reservedOp frl "0i"; try (do {n <- parsePot; return (compOp (Op LeftZero) n)}) <|> return (Op LeftZero)})
          <|> (do {reservedOp frl "0d"; try (do {n <- parsePot; return (compOp (Op RightZero) n)}) <|> return (Op RightZero)})
          <|> (do {reservedOp frl "Bi"; try (do {n <- parsePot; return (compOp (Op LeftDel) n)}) <|> return (Op LeftDel)})
          <|> (do {reservedOp frl "Bd"; try (do {n <- parsePot; return (compOp (Op RightDel) n)}) <|> return (Op RightDel)})
          <|> (do {reservedOp frl "Si"; try (do {n <- parsePot; return (compOp (Op LeftSucc) n)}) <|> return (Op LeftSucc)})
          <|> (do {reservedOp frl "Sd"; try (do {n <- parsePot; return (compOp (Op RightSucc) n)}) <|> return (Op RightSucc)})
          <|> (do {reservedOp frl "Di"; try (do {n <- parsePot; return (compOp (Op DupLeft) n)}) <|> return (Op DupLeft)})
          <|> (do {reservedOp frl "Dd"; try (do {n <- parsePot; return (compOp (Op DupRight) n)}) <|> return (Op DupRight)})
          <|> (do {reservedOp frl "(<->)"; try (do {n <- parsePot; return (compOp (Op Swap) n)}) <|> return (Op Swap)})
          <|> (do {reservedOp frl "(<-)"; try (do {n <- parsePot; return (compOp (Op MoveLeft) n)}) <|> return (Op MoveLeft)})
          <|> (do {reservedOp frl "(->)"; try (do {n <- parsePot; return (compOp (Op MoveRight) n)}) <|> return (Op MoveRight)})
          <|> parseRepeat
          <|> parseCompPot

compOp :: Fun -> Integer -> Fun
compOp op 1 = op
compOp op n = Comp op (compOp op (n - 1))

parseRepeat :: Parser Fun
parseRepeat = do reservedOp frl "<"
                 f <- fun
                 reservedOp frl ">"
                 return (Repeat f)

parseCompPot :: Parser Fun
parseCompPot = do reservedOp frl "{"
                  f <- fun
                  reservedOp frl "}"
                  n <- natural frl
                  return (compOp f n)

parsePot :: Parser Integer
parsePot = do n <- natural frl
              if n > 0 then return n else fail "parsePot error"

-- Parser de comandos

comm :: Parser Comm
comm = chainl1 comm' (do {reservedOp frl ";"; return Seq})

comm' :: Parser Comm
comm' = try parseSkip <|> try parseLet <|> try parseEq <|> try parseShow <|> parseApp

parseSkip :: Parser Comm
parseSkip = do reserved frl "skip"
               return Skip

parseLet :: Parser Comm
parseLet = do v <- identifier frl
              reservedOp frl "="
              try (do {guard (length v == 1 && isUpper (head v)); f <- fun; ls <- list; return (LetListFun v f ls)})
                   <|> (do {guard (length v == 1 && isUpper (head v)); ls <- list; return (LetList v ls)}
                   <|> (do {guard (length v > 1 && isLower (head v)); f <- fun; return (LetFun v f)}))

parseEq :: Parser Comm
parseEq = do f <- fun
             xs <- list
             op <- (reservedOp frl "==" >> return Eq) <|> (reservedOp frl "!=" >> return NEq)
             ys <- list
             return (op f xs ys)

parseShow :: Parser Comm
parseShow = do reserved frl "print"
               v <- identifier frl
               return (Print v)
               

parseApp :: Parser Comm
parseApp = do f <- fun
              ls <- list
              return (App f ls)

-- Función de parseo

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

