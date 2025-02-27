module Main where

import AST
import Eval
import Parser ( parseComm )
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )

main :: IO ()
main = putStrLn "Hello, Haskell!"
