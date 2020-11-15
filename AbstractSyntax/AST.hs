module AbstractSyntax.AST where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

-----------------------------------------------------------------------------------------
-- * Domains
-----------------------------------------------------------------------------------------

type VariableName = String   -- ^ Variables
type Value        = Integer  -- ^ The semantic domain is Integers
type LineNumber   = Integer  -- ^ Line numbers are integers

-----------------------------------------------------------------------------------------
-- * Statements 
-----------------------------------------------------------------------------------------

data Statement = Assign VariableName Term            -- ^ x := term
               | AssignExpr VariableName ArithExpr   -- ^ x := arith
               | Input VariableName                  -- ^ input x
               | Print VariableName                  -- ^ print x
               | Goto LineNumber                     -- ^ goto l
               | If Condition LineNumber             -- ^ if condition goto l
               | Skip                                -- ^ skip
               | Halt                                -- ^ halt
  deriving (Eq, Ord)

-----------------------------------------------------------------------------------------
-- * Terms 
-----------------------------------------------------------------------------------------

-- | A term is either a variable or a number
data Term = Var VariableName
          | Number Value
  deriving (Eq, Ord)

-----------------------------------------------------------------------------------------
-- * Arithmetic Expressions 
-----------------------------------------------------------------------------------------

-- | An arithmetic expression performs an arithmetic operation on two terms
data ArithExpr = AE Term AOp Term
  deriving (Eq, Ord)

-- | The arithmetic operations are: addition, subtraction, multiplication, and division
data AOp = Plus | Minus | Times | Divide
  deriving (Eq, Ord)

-----------------------------------------------------------------------------------------
-- * Conditions 
-----------------------------------------------------------------------------------------

-- | A condition compares the value of a variable to the value of a term
data Condition = Cond VariableName ROp Term
  deriving (Eq, Ord)

-- | The comparison operations are: equals and less-than
data ROp = Equal | LessThan
  deriving (Eq, Ord)

-----------------------------------------------------------------------------------------
-- * Programs 
-----------------------------------------------------------------------------------------

-- | A program maps line numbers to statements
newtype Program = Program (Map LineNumber Statement)
  deriving (Eq)

-- | Construct a program from a list of statements
mkProgram :: [Statement] -> Program
mkProgram statements =              
  let lineNumbers = [1..(fromIntegral . length) statements]
      pairs = zip lineNumbers statements in
    foldl addStatement blankProgram pairs

-- | A program with no lines
blankProgram :: Program
blankProgram = Program Map.empty

-- | Get a statement at a given lineNumber
getStatement :: Program -> LineNumber -> Statement
getStatement (Program p) l = Map.findWithDefault (error ("Unknown line number " ++ show l)) l p

-- | Add a statement at a given lineNumber (overwrites any existing statement on that line)
addStatement :: Program -> (LineNumber, Statement) ->  Program
addStatement (Program p) (l, s) = Program $ Map.insert l s p

-- | Get all the statements in a program, paired with their corresponding line numbers
getLines :: Program -> [(LineNumber, Statement)]
getLines (Program p)= Map.assocs p

-----------------------------------------------------------------------------------------
-- Showing ASTs 
-----------------------------------------------------------------------------------------

instance Show Program where
  show p = 
    let programLines = sortBy (\ (l1, _) (l2, _) -> compare l1 l2) (getLines p)
        statements = map snd programLines in
          intercalate "\n" (map show statements)

instance Show Statement where
  show (Assign name term)       = name ++ " := " ++ show term
  show (AssignExpr name arith)  = name ++ " := " ++ show arith
  show (Input name)             = "input " ++ name
  show (Print name)             = "print " ++ name
  show (Goto l)                 = "goto "  ++ show l
  show (If cond l)              = "if " ++ show cond ++ " goto " ++ show l
  show Skip                     = "skip"
  show Halt                     = "halt"

instance Show Term where
  show (Var name) = name
  show (Number n) = show n

instance Show ArithExpr where
  show (AE left op right) = show left ++ show op ++ show right

instance Show AOp where
  show Plus   = " + "
  show Minus  = " - "
  show Times  = " * "
  show Divide = " / "

instance Show Condition where
  show (Cond name op term) = name ++ show op ++ show term 

instance Show ROp where
  show Equal    = " == "
  show LessThan = " < "
