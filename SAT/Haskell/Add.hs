module Add where
import           Formula
import           Tseitin

ha :: Formula a -> Formula a -> (Formula a, Formula a)                 
ha x y = (x `Xor` y, x `And` y)

va :: Formula a -> Formula a -> Formula a -> (Formula a, Formula a)                 
va x y c =
  let (yc, d) = ha y c 
      (s, e) = ha x yc  
      cr = e `Or` d
  in (s, cr)

va2 :: Formula a -> Formula a -> Formula a -> (Formula a, Formula a)                 
va2 x y c = 
  let s = (x `Xor` y) `Xor` c 
      cr = ((c `And` y) `Or` (y `And` x)) `Or` (c `And` x)
  in (s, cr)

equiv :: Formula String
equiv =
  let (s1, c1) = va (Var "x") (Var "y") (Var "c")
      (s2, c2) = va2 (Var "x") (Var "y") (Var "c")
  in (s1 `Iff` s2) `And` (c1 `Iff` c2)

main :: IO ()
main = do
  answer <- satisfiable (Neg equiv)
  case answer of
    Unsatisfiable -> putStr "va und va2 sind sequivalent!"
    Satisfiable eta -> putStr ("va und va2 sind verschieden bei Belegung "
                               ++ show eta)
