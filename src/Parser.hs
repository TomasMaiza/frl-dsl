module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

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
    , reservedNames   = ["skip"]
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
                        , "<->"
                        , "Di"
                        , "Dd"
                        , "->"
                        , "<-"
                        ]
    }
  )

-- Parser de listas

list :: Parser List
list = brackets frl (try parseElems
                         <|> return Nil) 

parseElem :: Parser List
parseElem = do x <- natural frl
               return $ Unit $ fromIntegral x

parseElems :: Parser List
parseElems = chainl1 parseElem (do {reservedOp frl ","; return Concat})

-- Parser de funciones

fun :: Parser Fun
fun = chainr1 parseOp (return Comp)

parseOp :: Parser Fun
parseOp = (do {reservedOp frl "0i"; return (Op LeftZero)})
          <|> (do {reservedOp frl "0d"; return (Op RightZero)})
          <|> (do {reservedOp frl "Bi"; return (Op LeftDel)})
          <|> (do {reservedOp frl "Bd"; return (Op RightDel)})
          <|> (do {reservedOp frl "Si"; return (Op LeftSucc)})
          <|> (do {reservedOp frl "Sd"; return (Op RightSucc)})
          <|> (do {reservedOp frl "Di"; return (Op DupLeft)})
          <|> (do {reservedOp frl "Dd"; return (Op DupRight)})
          <|> (do {reservedOp frl "<->"; return (Op Swap)})
          <|> (do {reservedOp frl "<-"; return (Op MoveLeft)})
          <|> (do {reservedOp frl "->"; return (Op MoveRight)})
          <|> (do {f <- parseRepeat; return f})

parseRepeat :: Parser Fun
parseRepeat = do reservedOp frl "<"
                 f <- fun
                 reservedOp frl ">"
                 return (Repeat f)

------------------------------------
-- Función de parseo
------------------------------------
{-
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
-}
