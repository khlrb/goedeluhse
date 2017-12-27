module Clause where

import Control.Monad
import Data.List

import Expression

data Lit = Plain Int
         | Negated Int
  deriving (Show, Eq, Ord)

type Clause = [Lit]

negateLit (Plain x) = Negated x
negateLit (Negated x) = Plain x

toLit :: E -> Maybe Lit
toLit (Var x)       = Just $ Plain x
toLit (Neg (Var x)) = Just $ Negated x
toLit _             = Nothing

toClause :: E -> Maybe Clause
toClause x = nub <$> f x
  where f (a `Or` b) = liftM2 (++) (toClause a) (toClause b)
        f e          = return <$> toLit e

fromExpr :: E -> Maybe [Clause]
fromExpr x = nub <$> (f . cnf) x
  where f (a `And` b) = liftM2 (++) (f a) (f b)
        f e           = (return . sort) <$> toClause e

