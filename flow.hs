{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Parser.Parser (parseFile, parseNamed)
import AbstractSyntax.AST
import Semantics.ConcreteSemantics
import Semantics.Dot
import Semantics.Domains

import           Data.Map (Map)
import qualified Data.Map as Map
import System.Console.CmdArgs
import System.Environment
import System.Exit
import Control.Monad

-----------------------------------------------------------------------------------------
-- Handle command-line arguments 
-----------------------------------------------------------------------------------------
data FlowArgs = Run { file :: FilePath, show_store :: Bool }
              | Dot { file :: FilePath }
  deriving (Show, Data, Typeable)

-- flow [run] [PROGRAM]
runArgs :: FlowArgs
runArgs =  Run { file = def &= argPos 0 &= typ "PROGRAM" &= opt ""
               , show_store = False }
        &= help "Run the program" 
        &= auto

-- flow dot [PROGRAM]
dotArgs :: FlowArgs
dotArgs =  Dot { file = def &= argPos 0 &= typ "PROGRAM" &= opt "" }  
        &= help "Render the program in dot notation"

mode = modes [runArgs, dotArgs] 
     &= program "flow"
     &= help "The flow programming language"

-----------------------------------------------------------------------------------------
-- Run the program 
-----------------------------------------------------------------------------------------
runProgram :: Program -> Bool -> IO ()
runProgram program displayStore = 
  do (_, σ) <- doProgram program
     when displayStore $
      print σ

-----------------------------------------------------------------------------------------
--  Transform the program to dot notation and display it
-----------------------------------------------------------------------------------------
printDot :: Program -> IO ()
printDot = putStrLn . makeDot

-----------------------------------------------------------------------------------------
-- Main 
-----------------------------------------------------------------------------------------
main :: IO ()
main = do 
          -- Process the command-line arguments
          args <- cmdArgs mode

          -- Parse the program in the user-supplied file
          -- or from stdin (if no filename is provided)
          let filename = file args
          program <- if null filename
                     then getContents >>= parseNamed "<stdin>"
                     else parseFile filename

          -- Run the command
          case args of
            Run{} -> runProgram program (show_store args)
            Dot{} -> printDot program
