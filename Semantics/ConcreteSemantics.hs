module Semantics.ConcreteSemantics where

import           AbstractSyntax.AST
import           Semantics.Domains
import           System.IO

-----------------------------------------------------------------------------------------
-- * Concrete Domains
-----------------------------------------------------------------------------------------
initialStore :: ConcreteStore
initialStore = emptyStore

-----------------------------------------------------------------------------------------
-- * Programs
-----------------------------------------------------------------------------------------

-- | Given a program, compute the transitive closure of the "next state" transition. If
--   the computation reaches a Halt statement, the result will be the line number for the
--   Halt statement and the final store.
doProgram :: Program -> IO (LineNumber, ConcreteStore)
doProgram p = loop (1, initialStore)

  where loop (l, σ) = case getStatement p l of

                        -- If this statement is Halt, we're done!
                        Halt -> return (l, σ)

                        -- Otherwise, take a step and use the results as input to
                        -- the next step
                        _    -> doStatement p (l, σ) >>= loop

-----------------------------------------------------------------------------------------
-- * Statement transitions
-----------------------------------------------------------------------------------------
doStatement :: Program -> (LineNumber, ConcreteStore) -> IO (LineNumber, ConcreteStore)
doStatement p (l, σ) =

  case getStatement p l of

    Halt      -> return (l,     σ)

    Skip      -> return (l + 1, σ)

    (Goto l') -> return (l',    σ)

    (Print x) -> do print (resolve x σ)
                    return (l + 1, σ)

    (Input x) -> do n <- prompt "type a number: "
                    return (l+1, σ // [x ↦ n])

    (Assign     x t) -> return (l+1, σ // [x ↦ evalTerm t σ])

    (AssignExpr x e) -> return (l+1, σ // [x ↦ evalExpr e σ])

    (If c l') -> if evalCond c σ
                   then return (l',  σ)
                   else return (l+1, σ)

-----------------------------------------------------------------------------------------
-- * Evaluating terms
-----------------------------------------------------------------------------------------
evalTerm :: Term -> ConcreteStore -> ConcreteValue
evalTerm (Var x)    σ = resolve x σ
evalTerm (Number n) _ = n

-----------------------------------------------------------------------------------------
-- * Evaluating arithmetic expressions
-----------------------------------------------------------------------------------------
evalExpr :: ArithExpr -> ConcreteStore -> ConcreteValue
evalExpr (AE term1 Plus term2)   σ = evalTerm term1 σ   +   evalTerm term2 σ
evalExpr (AE term1 Minus term2)  σ = evalTerm term1 σ   -   evalTerm term2 σ
evalExpr (AE term1 Times term2)  σ = evalTerm term1 σ   *   evalTerm term2 σ
evalExpr (AE term1 Divide term2) σ = evalTerm term1 σ `div` evalTerm term2 σ

-----------------------------------------------------------------------------------------
-- * Evaluating conditions
-----------------------------------------------------------------------------------------
evalCond :: Condition -> ConcreteStore -> Bool
evalCond (Cond x Equal t)    σ = resolve x σ == evalTerm t σ
evalCond (Cond x LessThan t) σ = resolve x σ <  evalTerm t σ


-----------------------------------------------------------------------------------------
-- * Helper functions
-----------------------------------------------------------------------------------------

-- From: https://stackoverflow.com/questions/13190314/io-happens-out-of-order-when-using-getline-and-putstr
prompt :: String -> IO Integer
prompt text = do
    putStr text
    hFlush stdout
    read <$> getLine
