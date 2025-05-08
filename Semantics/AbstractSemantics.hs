module Semantics.AbstractSemantics where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import           AbstractSyntax.AST
import           Semantics.Domains

import Debug.Trace
import Text.Printf


-----------------------------------------------------------------------------------------
-- * Abstract domains
-----------------------------------------------------------------------------------------
type AbstractValue = AbstractInt
type AbstractStore = Store AbstractValue
type AbstractState = Map LineNumber AbstractStore
type StepState = (AbstractState, Set LineNumber)

initialStore :: AbstractStore
initialStore = emptyStore

-- | Produce an analysis of the program
doAnalysis :: Program -> String
doAnalysis p = unlines $ map (\(l, s) -> fmt l ++ " " ++ show s) $ Map.toList . fst $ analysis-- unlines $ map (\l s -> fmt l) $ fst $ analysisn 6 -- unlines $ map (\(k,a) -> show k ++ show a) (Map.toList (fst analysis))
   where
      analysis = fixpoint (doAnalysisStep p) (Map.fromList [(1, emptyStore)], Set.fromList [1])
      analysisn n = iterate (doAnalysisStep p) (Map.fromList [(1, emptyStore)], Set.fromList [1]) !! n

doAnalysisStep :: Program -> StepState -> StepState
doAnalysisStep p (s, ls) = foldl combineSteps (Map.empty, Set.empty) (map (doAnalysisSingleStep p s) $ Set.toList ls)
   where
      -- perform the join operation on the store for each line in the abstractstate
      -- perform a union of the two lists of line numbers
      combineSteps (a1, l1) (a2, l2) = (Map.unionWith join a1 a2, Set.union l1 l2)

doAnalysisSingleStep :: Program -> AbstractState -> LineNumber -> StepState
doAnalysisSingleStep p s l =
   case getStatement p l of
      Halt      -> transition s [] l --(s', nextLine l)

      Skip      -> transition s [] (l+1) --(s', nextLine $ l+1)

      (Goto l') -> transition s [] l' --(s', nextLine l')

      (Print _) -> transition s [] (l+1) --(s', nextLine $ l+1)

      (Input x) -> transition s [x ↦ (⊤)] (l+1)--(insertToAbstractState s' [x ↦ (⊤)], nextLine $ l+1)

      (Assign     x t) -> transition s [x ↦ analyzeTerm t lineStore] (l+1) --(insertToAbstractState s' [x ↦ analyzeTerm t lineStore], nextLine $ l+1)
      
      (AssignExpr x e) -> transition s [x ↦ analyzeExpr e lineStore] (l+1) --(insertToAbstractState s' [x ↦ analyzeExpr e lineStore], nextLine $ l+1)

   where
      -- generate transition to the next line (add the current store to the store for the next line, and make a list of the next lines)
      transition abstractState bind nl = (insertToAbstractState s bind nl, Set.fromList [nl])
      -- extract the store for the current line from the abstract state
      lineStore = Map.findWithDefault emptyStore l s
      -- insert any changes due to current line into the abstract state for the next line, using the store for teh current line as a basis
      insertToAbstractState abstractState bind nl = Map.insertWith join nl (lineStore // bind) abstractState

analyzeTerm :: Term -> AbstractStore -> AbstractValue
analyzeTerm (Var x)    s = resolve x s
analyzeTerm (Number n) s = Element n

analyzeExpr :: ArithExpr -> AbstractStore -> AbstractValue
analyzeExpr (AE term1 Plus term2)   σ = analyzeTerm term1 σ   `abstractAdd`   analyzeTerm term2 σ
analyzeExpr (AE term1 Minus term2)  σ = analyzeTerm term1 σ   `abstractSub`   analyzeTerm term2 σ
-- analyzeExpr (AE term1 Times term2)  σ = analyzeTerm term1 σ   *   analyzeTerm term2 σ
-- analyzeExpr (AE term1 Divide term2) σ = analyzeTerm term1 σ `div` analyzeTerm term2 σ

abstractAdd :: AbstractValue -> AbstractValue -> AbstractValue
abstractAdd Top         _           = (⊤)
abstractAdd _           Top         = (⊤)
abstractAdd (Element l) (Element r) = (⊤)
abstractAdd Bottom      Bottom      = (⊥)

abstractSub :: AbstractValue -> AbstractValue -> AbstractValue
abstractSub Top         _           = (⊤)
abstractSub _           Top         = (⊤)
abstractSub (Element l) (Element r) = (⊤)
abstractSub Bottom      Bottom      = (⊥)

-----------------------------------------------------------------------------------------
-- * Helper functions
-----------------------------------------------------------------------------------------
-- From: https://stackoverflow.com/a/32311606
fmt :: Integer -> String
fmt = printf "%02d"