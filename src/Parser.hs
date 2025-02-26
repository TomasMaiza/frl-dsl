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
list = brackets frl (try parseElem
                         <|> return Nil)

parseElem :: Parser List
parseElem = do x <- natural frl
               return (Unit $ fromIntegral x)

------------------------------------
-- Función de parseo
------------------------------------
{-
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
-}
