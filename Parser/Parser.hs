module Parser.Parser (parse, parseFile, parseNamed, program) where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 

import AbstractSyntax.AST

--------------------------------------------------------------------------------
-- * Parsing functions
--------------------------------------------------------------------------------

-- | Parse a Flow program from a file
parseFile :: FilePath -> IO Program
parseFile filename = readFile filename >>= parseNamed filename

-- | Parse a Flow program, given a name for the source of the program and a string that
--   contains the contents of the programs
parseNamed :: String -> String -> IO Program
parseNamed name contents = 
  case parse program name contents of
    Right ast -> return ast
    Left err  -> errorWithoutStackTrace (show err)


-----------------------------------------------------------------------------------------
-- Basic domains: variables, line numbers, and values 
-----------------------------------------------------------------------------------------
identifier = Token.identifier lexer     -- ^  x ∈ Variables
lineNumber = Token.natural lexer        -- ^  l ∈ Line numbers 
number     = Token.integer lexer        -- ^  n ∈ ℤ


--------------------------------------------------------------------------------
-- * Statements
--------------------------------------------------------------------------------

-- | s ∈ Statements ::= x := arith
--                   |  x := term
--                   |  input x
--                   |  print x
--                   |  goto l
--                   |  if cond goto l
--                   |  skip
--                   |  halt
statement :: Parsec String () Statement
statement =  try assignExpr
         <|> assign
         <|> ifStatement
         <|> (keyword "input" *> identifier >>=: Input)   -- input x
         <|> (keyword "print" *> identifier >>=: Print)   -- print x
         <|> (keyword "goto"  *> lineNumber >>=: Goto)    -- goto l
         <|> (keyword "skip"                >>:  Skip)    -- skip
         <|> (keyword "halt"                >>:  Halt)    -- halt
         <?> "expected statement"

-- | x := arith
assignExpr :: Parsec String () Statement
assignExpr = do var <- identifier
                op ":="
                expr <- arithExpr
                let ast = AssignExpr var expr
                return ast

-- | x := term
assign :: Parsec String () Statement
assign = do var <- identifier
            op ":="
            value <- term
            let ast = Assign var value
            return ast

-- | if cond goto l
ifStatement :: Parsec String () Statement
ifStatement = do keyword "if"
                 cond <- condition
                 keyword "goto"
                 lineNum <- lineNumber
                 let ast = If cond lineNum
                 return ast

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

-- | term ∈ Terms ::= x | n
term :: Parsec String () Term
term =  (identifier >>=: Var)
    <|> (number     >>=: Number)

--------------------------------------------------------------------------------
-- Arithmetic expressions
--------------------------------------------------------------------------------

-- | arith ∈ Arithmetic expressions ::= term aop term
arithExpr :: Parsec String () ArithExpr
arithExpr =
  do left <- term
     op <- arithOp
     right <- term
     let ast = AE left op right
     return ast

-- | aop ∈ Arithmetic operators ::= + | - | * | /
arithOp :: Parsec String () AOp
arithOp =  (op "+" >>: Plus)
       <|> (op "-" >>: Minus)
       <|> (op "*" >>: Times)
       <|> (op "/" >>: Divide)

--------------------------------------------------------------------------------
-- Conditions
--------------------------------------------------------------------------------

-- | cond ∈ Conditional expressions ::= x rop term
condition :: Parsec String () Condition
condition = 
  do var <- identifier
     op  <- relativeOp
     operand <- term
     let ast = Cond var op operand
     return ast

-- | rop ∈ Relational operators ::= == | <
relativeOp :: Parsec String () ROp
relativeOp =  (op "==" >>: Equal)
          <|> (op "<"  >>: LessThan)

--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

-- | p ∈ Programs ::= [s1, s2, ..., sn]
program :: Parsec String () Program
program = do statements <- many statement
             eof
             return (mkProgram statements)

-----------------------------------------------------------------------------------------
-- Convenience parsers
-----------------------------------------------------------------------------------------
keyword = Token.reserved lexer
op      = Token.reservedOp lexer

infixl 1 >>=:
left >>=: f = f <$> left

infixl 1 >>:
left >>: v = left >> return v

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------
langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":=;+-*/<"
  , Token.opLetter        = oneOf "=;+-*/<"
  , Token.reservedNames   = ["goto", "if", "skip", "halt", "input", "print"]
  , Token.reservedOpNames = [  ":="
                             , "+", "-", "*", "/"
                             , "==", "<"]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef
