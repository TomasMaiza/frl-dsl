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
    , reservedNames   = ["mode"]
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
                        ]
    }
  )

-- Parser de listas

list :: Parser List
list = do parseVar
       <|> brackets frl (try parseElems
                         <|> return Nil) 

parseNat :: Parser List
parseNat = do x <- natural frl
              return $ Unit $ fromIntegral x

parseElems :: Parser List
parseElems = chainl1 (do {parseNat <|> parseVar}) (do {reservedOp frl ","; return Concat})

parseVar :: Parser List
parseVar = do v <- identifier frl
              return (Var v)


-- Parser de funciones

fun :: Parser Fun
fun = chainr1 parseOp (return Comp)

parseOp :: Parser Fun
parseOp = (do {reservedOp frl "0i"; try (do {n <- natural frl; return (compOp (Op LeftZero) n)}) <|> return (Op LeftZero)})
          <|> (do {reservedOp frl "0d"; try (do {n <- natural frl; return (compOp (Op RightZero) n)}) <|> return (Op RightZero)})
          <|> (do {reservedOp frl "Bi"; try (do {n <- natural frl; return (compOp (Op LeftDel) n)}) <|> return (Op LeftDel)})
          <|> (do {reservedOp frl "Bd"; try (do {n <- natural frl; return (compOp (Op RightDel) n)}) <|> return (Op RightDel)})
          <|> (do {reservedOp frl "Si"; try (do {n <- natural frl; return (compOp (Op LeftSucc) n)}) <|> return (Op LeftSucc)})
          <|> (do {reservedOp frl "Sd"; try (do {n <- natural frl; return (compOp (Op RightSucc) n)}) <|> return (Op RightSucc)})
          <|> (do {reservedOp frl "Di"; try (do {n <- natural frl; return (compOp (Op DupLeft) n)}) <|> return (Op DupLeft)})
          <|> (do {reservedOp frl "Dd"; try (do {n <- natural frl; return (compOp (Op DupRight) n)}) <|> return (Op DupRight)})
          <|> (do {reservedOp frl "(<->)"; try (do {n <- natural frl; return (compOp (Op Swap) n)}) <|> return (Op Swap)})
          <|> (do {reservedOp frl "(<-)"; try (do {n <- natural frl; return (compOp (Op MoveLeft) n)}) <|> return (Op MoveLeft)})
          <|> (do {reservedOp frl "(->)"; try (do {n <- natural frl; return (compOp (Op MoveRight) n)}) <|> return (Op MoveRight)})
          <|> (do {f <- parseRepeat; return f})

compOp :: Fun -> Integer -> Fun
compOp op 1 = op
compOp op n = Comp op (compOp op (n - 1)) 

{-
parseOp :: Parser Fun
parseOp = (do {reservedOp frl "0i"; try (do {n <- natural frl; return (Op LeftZero $ fromIntegral n)}) <|> return (Op LeftZero 1)})
          <|> (do {reservedOp frl "0d"; try (do {n <- natural frl; return (Op RightZero $ fromIntegral n)}) <|> return (Op RightZero 1)})
          <|> (do {reservedOp frl "Bi"; try (do {n <- natural frl; return (Op LeftDel $ fromIntegral n)}) <|> return (Op LeftDel 1)})
          <|> (do {reservedOp frl "Bd"; try (do {n <- natural frl; return (Op RightDel $ fromIntegral n)}) <|> return (Op RightDel 1)})
          <|> (do {reservedOp frl "Si"; try (do {n <- natural frl; return (Op LeftSucc $ fromIntegral n)}) <|> return (Op LeftSucc 1)})
          <|> (do {reservedOp frl "Sd"; try (do {n <- natural frl; return (Op RightSucc $ fromIntegral n)}) <|> return (Op RightSucc 1)})
          <|> (do {reservedOp frl "Di"; try (do {n <- natural frl; return (Op DupLeft $ fromIntegral n)}) <|> return (Op DupLeft 1)})
          <|> (do {reservedOp frl "Dd"; try (do {n <- natural frl; return (Op DupRight $ fromIntegral n)}) <|> return (Op DupRight 1)})
          <|> (do {reservedOp frl "(<->)"; try (do {n <- natural frl; return (Op Swap $ fromIntegral n)}) <|> return (Op Swap 1)})
          <|> (do {reservedOp frl "(<-)"; try (do {n <- natural frl; return (Op MoveLeft $ fromIntegral n)}) <|> return (Op MoveLeft 1)})
          <|> (do {reservedOp frl "(->)"; try (do {n <- natural frl; return (Op MoveRight $ fromIntegral n)}) <|> return (Op MoveRight 1)})
          <|> (do {f <- parseRepeat; return f}) -}

parseRepeat :: Parser Fun
parseRepeat = do reservedOp frl "<"
                 f <- fun
                 reservedOp frl ">"
                 return (Repeat f)


-- Parser de comandos

comm :: Parser Comm
comm = chainl1 comm' (do {reservedOp frl ";"; return Seq})

comm' :: Parser Comm
comm' = try parseSkip <|> try parseLet <|> try parseEq <|> parseApp

parseSkip :: Parser Comm
parseSkip = do reserved frl "skip"
               return Skip

parseLet :: Parser Comm
parseLet = do v <- identifier frl
              reservedOp frl "="
              try (do {f <- fun; ls <- list; return (LetListFun v f ls)})
                   <|> (do {ls <- list; return (LetList v ls)})

parseEq :: Parser Comm
parseEq = do f <- fun
             xs <- list
             op <- (reservedOp frl "==" >> return Eq) <|> (reservedOp frl "!=" >> return NEq)
             ys <- list
             return (op f xs ys)

parseApp :: Parser Comm
parseApp = do f <- fun
              ls <- list
              return (App f ls)

-- Función de parseo

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

