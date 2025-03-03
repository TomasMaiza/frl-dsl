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


use :: String
use = "Use: cabal run tp -- pathToFile opt\ncabal run tp -- -i\nopt = -i (interactive), -s (static)"


main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [input] -> case input !! 0 of
                '-' -> case input of
                        "-i" -> do putStrLn "Entering interactive mode. Enter :q to exit."
                                   interactiveLoop 0 initEnv
                        _ -> putStrLn use
                _ -> do content <- readFile input
                        case parseComm input content of
                          Left err -> putStrLn $ "Parsing error:\n" ++ show err
                          Right ast -> case eval ast noTrace of
                                          Left err -> putStrLn $ renderError err
                                          Right (_, trace) -> putStrLn $ renderTrace trace
    [filename, opt] -> do input <- readFile filename
                          case parseComm filename input of
                            Left err -> putStrLn $ "Parsing error:\n" ++ show err
                            Right ast -> case opt of
                                          "-s" -> case eval ast noTrace of
                                                    Left err -> putStrLn $ renderError err
                                                    Right (_, trace) -> putStrLn $ renderTrace trace
                                          "-i" -> case eval ast interactive of
                                                    Left err -> putStrLn $ renderError err
                                                    Right (_, trace) -> putStrLn $ renderTrace trace
                                          _ -> putStrLn use
    _ -> putStrLn use

interactiveLoop :: Int -> Env -> IO ()
interactiveLoop line e = do putStr "> " 
                            hFlush stdout
                            input <- getLine
                            case words input of
                              [":q"] -> putStrLn "Bye!"
                              (":l":filename:_) -> do env <- useFile filename interactive e
                                                      interactiveLoop (line + 1) env
                              _ -> do case parseComm "<stdin>" input of
                                        Left err -> do putStrLn $ "Parsing error: " ++ show err
                                                       interactiveLoop (line + 1) e
                                        Right ast -> if line == 0
                                                     then case eval ast interactive of
                                                            Left err -> do putStrLn $ renderError err
                                                                           interactiveLoop (line + 1) e
                                                            Right (env, trace) -> do putStrLn $ renderTrace trace
                                                                                     interactiveLoop (line + 1) env
                                                     else case intEval ast e of
                                                            Left err -> do putStrLn $ renderError err
                                                                           interactiveLoop (line + 1) e
                                                            Right (env, trace) -> do putStrLn $ renderTrace trace
                                                                                     interactiveLoop (line + 1) env

useFile :: String -> Mode -> Env -> IO Env
useFile filename mode e = do input <- readFile filename
                             case parseComm filename input of
                                Left err -> do putStrLn $ "Parsing error:\n" ++ show err
                                               return e
                                Right ast -> case eval ast mode of
                                                Left err -> do putStrLn $ renderError err
                                                               return e
                                                Right (env, trace) -> do putStrLn $ renderTrace trace
                                                                         return env
