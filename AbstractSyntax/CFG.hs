module AbstractSyntax.CFG (CFG, fromAST, root, successors, nodes, edges) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

import AbstractSyntax.AST

-- | The type of a node in the CFG 
type NodeType = (LineNumber, Statement)

-- | A control-flow-graph for a program
newtype CFG = CFG (Map NodeType [NodeType])
  deriving (Eq, Show)

-- | Convert an AST to a CFG
fromAST :: Program -> CFG
fromAST program = CFG $ Map.fromList [(p, nexts p) | p <- getLines program]
  where nexts :: NodeType -> [NodeType]
        nexts (l, s) = [(l', getStatement program l') | l' <- lineSuccessors (l, s)]

-- | Convert a CFG to an AST
toAST :: CFG -> Program
toAST = mkProgram . map snd . sort . nodes

-- | The root of the CFG
root :: CFG -> NodeType
root (CFG m) = head (filter (\k -> fst k == 1) (Map.keys m))

-- | The nodes of the CFG
nodes :: CFG -> [NodeType]
nodes (CFG m) = Map.keys m

-- | The edges of the CFG
edges :: CFG -> [(NodeType, NodeType)]
edges (CFG m) = [(n, n') | (n, ns) <- Map.assocs m, n' <- ns]

-- | The successors of a node in the CFG
successors :: CFG -> NodeType -> [NodeType]
successors (CFG m) n = Map.findWithDefault (error ("Unknown node" ++ show n)) n m

--  Given a line number and a statement, computes the list of line numbers that can come
--  next 
lineSuccessors :: NodeType -> [LineNumber]
lineSuccessors (_, Goto l') = [l']
lineSuccessors (l, If _ l') = [l', l+1]
lineSuccessors (_, Halt)    = []
lineSuccessors (l, _)       = [l+1]
