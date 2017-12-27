module Expression where

data E = Var Int | E `And` E | E `Or` E | Neg E | E `Impl` E | E `Equiv` E
  deriving (Eq)

instance Show E where
  show (Var x) = 'X' : show x
  show (x `And` y) = "(" ++ show x ++ " ∧ " ++ show y ++ ")"
  show (x `Or` y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Neg x) = "¬" ++ show x
  show (x `Impl` y) = "(" ++ show x ++ " ⇒ " ++ show y ++ ")"
  show (x `Equiv` y) = "(" ++ show x ++ " ⇔ " ++ show y ++ ")"



cnf :: E -> E

