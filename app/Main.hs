module Main where

import AST
import Eval
import Parser ( parseComm )
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filename] -> do input <- readFile filename
                     case parseComm filename input of
                      Left err -> putStrLn $ "Error de parsing:\n" ++ show err
                      Right ast -> do let result = eval ast
                                      print result
    _ -> putStrLn "Uso: ./programa archivo.frl"
