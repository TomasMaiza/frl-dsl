module Test where

import AST
import Eval
import Parser ( parseComm, list, fun, comm )
import PPfrl
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )
import           System.IO (hFlush, stdout)
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

parserTest :: Bool
parserTest = and [parserListTest, parserFunTest, parseCommTest]

parserListTest :: Bool
parserListTest = and [ parse list "" "[]" == Right Nil,
                       parse list "" "[1]" == Right (Unit 1),
                       parse list "" "X" == Right (Var "X"),
                       parse list "" "[1, 2, 3]" == Right (Concat (Concat (Unit 1) (Unit 2)) (Unit 3))
                     ]

parserFunTest :: Bool
parserFunTest = and [ parse fun "" "foo" == Right (FunVar "foo"),
                      parse fun "" "0d" == Right (Op RightZero),
                      parse fun "" "0d 3" == Right (Comp (Op RightZero) (Comp (Op RightZero) (Op RightZero))),
                      parse fun "" "< Si Sd >" == Right (Repeat (Comp (Op LeftSucc) (Op RightSucc))),
                      parse fun "" "{< Si Sd >} 2" == Right (Comp (Repeat (Comp (Op LeftSucc) (Op RightSucc))) 
                                                                  (Repeat (Comp (Op LeftSucc) (Op RightSucc))))
                    ]

parseCommTest :: Bool
parseCommTest = and [ parseComm "" "skip" == Right Skip,
                      parseComm "" "X = []; func = 0d" == Right (Seq (LetList "X" Nil) (LetFun "func" (Op RightZero))),
                      parseComm "" "X = [1]; print X; 0i X" == Right (Seq (Seq (LetList "X" (Unit 1)) (Print "X")) 
                                                                          (App (Op LeftZero) (Var "X")))
                    ]