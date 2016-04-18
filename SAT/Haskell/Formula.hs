module Formula (
  Formula(..)
) where

data Formula a =
    Var a
  | TT
  | FF
  | Neg (Formula a)
  | Or (Formula a) (Formula a)
  | And (Formula a) (Formula a)
  | Xor (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  | BigOr [Formula a]
  | BigAnd [Formula a]
    deriving (Show, Eq, Ord)
