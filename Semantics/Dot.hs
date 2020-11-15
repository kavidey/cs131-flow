module Semantics.Dot where

import AbstractSyntax.AST
import AbstractSyntax.CFG

-- | Produce a dot graph from a program
makeDot :: Program -> String
makeDot p = unlines $ 
     ["Digraph G {"] 
  ++    [indent ++ "node [shape=box]"]
  ++    map edge (edges cfg)
  ++    map node (nodes cfg) 
  ++ ["}"]
  where cfg = fromAST p

-- | Given a line number and a statement, produce a node description for that statement
node :: (LineNumber, Statement) -> String
node (l, s) = indent ++ show l ++ " [label = " ++ nodeLabel ++ "];"
  where nodeLabel = "\"[" ++ show l ++ "] " ++ show s ++ "\""

-- | Given two line numbers and a label, produce a labeled edge description for the edge
--   between the two line numbers
labeledEdge :: LineNumber -> LineNumber -> String -> String
labeledEdge l1 l2 label = indent ++ show l1 ++ " -> " ++ show l2 ++ edgeLabel
  where edgeLabel = if null label then "" else " [label = " ++ label ++ "];"

-- | Given a line number and a statement, produce a list of edge descriptions for all the
--   successors of that statement
edge :: ((LineNumber, Statement), (LineNumber, Statement)) -> String
edge ((l1, If _ l'), (l2, _))
  | l2 == l'  = labeledEdge l1 l2 "T"
  | otherwise = labeledEdge l1 l2 "F"
edge ((l1, _), (l2, _)) = labeledEdge l1 l2 ""

-- Helper function to indent the output by one level
indent :: String
indent = "\t"
