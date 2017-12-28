module Clause where

import Control.Monad
import Data.Set (fromList, delete, union)
import Data.Foldable (find)
import Data.Maybe (catMaybes)
import Expression

data Lit = Plain Int
         | Negated Int
  deriving (Show, Eq, Ord)

type Clause = Set Lit

negateLit (Plain x) = Negated x
negateLit (Negated x) = Plain x

toLit :: E -> Maybe Lit
toLit (Var x)       = Just $ Plain x
toLit (Neg (Var x)) = Just $ Negated x
toLit _             = Nothing

toClause :: E -> Maybe Clause
toClause (a `Or` b) = liftM2 union (toClause a) (toClause b)
toClause e          = singleton <$> toLit e

fromCNF :: E -> Maybe (Set Clause)
fromCNF (a `And` b) = liftM2 union (fromCNF a) (fromCNF b)
fromCNF e           = singleton <$> toClause e

resolveClause :: Clause -> Clause -> Maybe Clause
resolveClause x y = (\a -> union (delete a x) (delete (negateLit a) y)) <$> find (\l -> (negateLit l) `member` y) x

resolveClauseSet :: Set Clause -> Set Clause
resolveClauseSet clauses = clauses `union` fromList [ a | Just a <- [ resolveClause l r | l <- toList $ clauses, r <- toList $ clauses, l /= r]]

resolve :: Set Clause -> Set Clause
resolve clauses = if (size resolution) /= (size clauses) then resolve resolution else resolution
  where resolution = resolveClauseSet clauses
