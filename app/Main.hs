module Main where

import AST
import Eval
import Parser ( parseComm )
import PPfrl
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )
import           System.IO (hFlush, stdout)

{-
main :: IO ()
main = putStrLn "hola" -}

use :: String
use = "Use: cabal run tp -- pathToFile opt\ncabal run tp -- -i\nopt = -i (interactive), -s (static)"


main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [input] -> case input !! 0 of
                '-' -> case input of
                        "-i" -> do putStrLn "Entering interactive mode. Enter :q to exit."
                                   repl
                        _ -> putStrLn use
                _ -> do content <- readFile input
                        case parseComm input content of
                          Left err -> putStrLn $ "Parsing error:\n" ++ show err
                          Right ast -> case eval ast noTrace of
                                          Left err -> putStrLn $ renderError err
                                          Right trace -> putStrLn $ renderTrace trace
    [filename, opt] -> do input <- readFile filename
                          case parseComm filename input of
                            Left err -> putStrLn $ "Parsing error:\n" ++ show err
                            Right ast -> case opt of
                                          "-s" -> case eval ast noTrace of
                                                    Left err -> putStrLn $ renderError err
                                                    Right trace -> putStrLn $ renderTrace trace
                                          "-i" -> case eval ast interactive of
                                                    Left err -> putStrLn $ renderError err
                                                    Right trace -> putStrLn $ renderTrace trace
                                          _ -> putStrLn use
    _ -> putStrLn use

-- Bucle interactivo
repl :: IO ()
repl = do
  putStr "frl> " 
  hFlush stdout
  input <- getLine
  case input of
    ":q" -> putStrLn "Bye!"
    _ -> do
      case parseComm "<stdin>" input of
        Left err -> putStrLn $ "Error de parsing: " ++ show err
        Right ast -> case eval ast interactive of
                       Left err -> putStrLn $ renderError err
                       Right trace -> putStrLn $ renderTrace trace
      repl  -- Volver a pedir entrada