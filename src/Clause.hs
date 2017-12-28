module Clause where

import Control.Monad
import Data.Set

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

fromExpr :: E -> Maybe (Set Clause)
fromExpr x = (f . cnf) x
  where f (a `And` b) = liftM2 (union) (f a) (f b)
        f e           = singleton <$> toClause e

