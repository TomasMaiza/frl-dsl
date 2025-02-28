module Main where

import AST
import Eval
import Parser ( parseComm )
import PPfrl
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )

{-
main :: IO ()
main = putStrLn "hola" -}

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filename] -> do input <- readFile filename
                     case parseComm filename input of
                      Left err -> putStrLn $ "Parsing error:\n" ++ show err
                      Right ast -> case eval ast noTrace of
                                    Left err -> putStrLn $ renderError err
                                    Right trace -> putStrLn $ renderTrace trace
    _ -> putStrLn "Use: cabal run tp -- pathToFile"
    -- podría hacer otro patrón que sea [filename, option]? y que option sea una bandera tipo -i

--  read-eval-print loop
{-
readevalprint :: [String] -> Bool -> String -> InputT IO ()
readevalprint args inter lfile =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state' { inter = True } -}