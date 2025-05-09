{-# LANGUAGE FlexibleInstances  #-}

{-
  Inspired by http://matt.might.net/articles/partial-orders/
-}

module Semantics.Domains where

import AbstractSyntax.AST

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

-----------------------------------------------------------------------------------------
-- * Concrete domains
-----------------------------------------------------------------------------------------
type ConcreteValue = Integer
type ConcreteStore = Store ConcreteValue
instance ValueStore ConcreteValue

-----------------------------------------------------------------------------------------
-- * Stores 
-----------------------------------------------------------------------------------------

-- | A store maps variables names to values
newtype Store a = Store (Map VariableName a)
  deriving (Eq, Ord)

-- | Show the contents of a store
instance Show a => Show (Store a) where
  show (Store e) = "{" ++ keys ++ "}"
    where  keys = intercalate ", " $ map showMapping (Map.keys e)
           showMapping key = show key ++ " ↦ " ++ show (fromJust (Map.lookup key e))

-- | The empty store
emptyStore :: Store a
emptyStore = Store Map.empty

-- | A unicode alias for the empty store
(∅) :: Store a
(∅) = emptyStore

-- | A value store knows how to resolve and bind variables in a store
class ValueStore a where

  -- | Look up a given variable name in a given store
  resolve :: VariableName -> Store a -> a
  resolve x (Store s) = 
    Map.findWithDefault (errorWithoutStackTrace ("Unknown variable " ++ x)) x s

  -- | Bind a variable name to a value, in a store
  bind :: VariableName -> a -> Store a -> Store a
  bind  x v (Store s) = Store (Map.insert x v s)

  -- | Mathematical alias, used as part of a binding update:
  --   σ // [x ↦ v] updates σ so that x is bound to v
  (//) :: Store a -> [(VariableName, a)] -> Store a
  σ // bindings = foldl (\ σ' (x, v) -> bind x v σ') σ bindings

  -- | Mathematical alias, used as part of a binding update
  (↦) :: VariableName -> a -> (VariableName, a)
  (↦) = (,)

-----------------------------------------------------------------------------------------
-- * Abstract Domains 
-----------------------------------------------------------------------------------------

-- | A lattice for the type a (which defines a set of values)
class Lattice a where

    -- | The least element in the domain
  (⊥) :: a

  -- | The greatest element in the domain
  (⊤) :: a

  -- | The "less-than-or-equal" relation
  (⊑) :: a -> a -> Bool

  -- | The "not less-than-or-equal" relation
  (⋢) :: a -> a -> Bool
  v1 ⋢ v2 = not (v1 ⊑ v2)

  -- | The "least upper bound" operator
  (⊔) :: a -> a -> a


  -- ** Text aliases: text equivalents of the symbolic values and functions
  
    -- | Equivalent to ⊥
  bottom :: a
  bottom = (⊥)

  -- | Equivalent to ⊤
  top :: a
  top = (⊤)

  -- | Equivalent to ⊑
  lte :: a -> a -> Bool
  lte = (⊑)

  -- | Equivalent to ⊔
  join :: a -> a -> a
  join = (⊔)


-----------------------------------------------------------------------------------------
-- * Flat lattices
-----------------------------------------------------------------------------------------

-- | A flat lattice is a way to construct a lattice from an existing type. 
--
--   For example, @FlatLattice Integer@ gives us the lattice
--
--               Top
--           /  / | \  \ 
--        ...  -1 0 1  ...
--            \ \ | /  /
--             Bottom
data FlatLattice a = Bottom       -- ^ The least element
                   | Top          -- ^ The greatest element
                   | Element a    -- ^ An element of a as a lattice value
  deriving (Eq)                   

-- Displaying FlatLattices
instance (Show a) => Show (FlatLattice a) where
  show Bottom = "⊥"
  show Top = "⊤"
  show (Element v) = "«" ++ show v ++ "»"

instance Ord a => Ord (FlatLattice a) where
  (<=) = (⊑)

-- Defining a FlatLattice as a Lattice
instance Eq a => Lattice (FlatLattice a) where
  (⊥) = Bottom
  (⊤) = Top

  Bottom ⊑ _ = True
  _ ⊑ Top = True
  (Element v1) ⊑ (Element v2) = v1 == v2
  _ ⊑ _ = False

  Bottom ⊔ right = right
  left ⊔ Bottom = left
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  (Element v1) ⊔ (Element v2) = if v1 == v2 then Element v1 else Top

-----------------------------------------------------------------------------------------
-- Other types are lattices, too!
-----------------------------------------------------------------------------------------

-- A Set is a Lattice
instance (Ord a, Eq a) => Lattice (Set a) where
  (⊥) = Set.empty
  (⊤) = error "There is no representation of the universal set"
  (⊑) = Set.isSubsetOf
  (⊔) = Set.union

-- A pair is a Lattice (if its elements are lattices)
instance (Lattice a, Lattice b) => Lattice (a, b) where
  (⊥) = ((⊥), (⊥))
  (⊤)    = ((⊤), (⊤))
  (a1, b1) ⊑ (a2, b2) = (a1 ⊑ a2) && (b1 ⊑ b2)
  (a1, b1) ⊔ (a2, b2) = (a1 ⊔ a2, b1 ⊔ b2) 
  
-- A Map is a Lattice (if they type of its values is a lattice)
instance (Ord a, Lattice b) => Lattice (Map a b) where
  (⊥) = Map.empty
  (⊤) = error "There is no representation of the top map"
  (⊑) = Map.isSubmapOfBy lte 
  (⊔) = Map.unionWith join

-- A Store is a lattice (if its values are lattices)
instance (Lattice a) => Lattice (Store a) where
  (⊥) = emptyStore
  (⊤) = error "There is no representation of the top store"
  (Store s1) ⊑ (Store s2) = s1 ⊑ s2
  (Store s1) ⊔ (Store s2) = Store (s1 ⊔ s2)

-- | Resolve a variable in an abstract store
--   If the variable is not defined in the store, the result is ⊥.
aresolve :: (Lattice b) => VariableName -> Map VariableName b -> b
aresolve = Map.findWithDefault (⊥)

-- | When storing to an abstract store, overwrite the existing value, if the existing
--   value is non-comparable
strongUpdate :: (Lattice a) => VariableName -> a -> Map VariableName a -> Map VariableName a
strongUpdate = Map.insertWithKey (const update)
  where update new old | (old ⋢ new) && (new ⋢ old) = new
                       | otherwise = new ⊔ old

-- | When storing to an abstract store, always join with the existing value
weakUpdate :: (Eq a, Lattice a) => VariableName -> a -> Map VariableName a -> Map VariableName a
weakUpdate = Map.insertWithKey (const (⊔))

-- | The default for abstract stores is weak update
instance Eq a => ValueStore (FlatLattice a) where
  resolve x (Store s) = aresolve x s
  bind x v (Store s) = Store $ weakUpdate x v s

-----------------------------------------------------------------------------------------
-- Misc 
-----------------------------------------------------------------------------------------

-- | A flat Bool lattice
type AbstractBool = FlatLattice Bool

-- | A flat Integer lattice
type AbstractInt = FlatLattice Integer

-- | Repeatedly apply a function to a value until it reaches a fixpoint
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = let x' = f x in if x' == x then x else fixpoint f x'

-- | Repeatedly apply a function to a value until it reaches a fixpoint
-- | A fix point is detected using a user-inputted equality checker
fixpointCustomEquality :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpointCustomEquality e f x = let x' = f x in if e x' x then x else fixpointCustomEquality e f x'