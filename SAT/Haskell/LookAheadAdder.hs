module Main where
import           Formula
import           Tseitin

--------------------------------                 
-- Ein-Bit Volladdierer

va_sum :: Formula a -> Formula a -> Formula a -> Formula a
va_sum x y c =
  (x `Xor` y) `Xor` c 
   
va_carry :: Formula a -> Formula a -> Formula a -> Formula a
va_carry x y c =
  ((c `And` y) `Or` (y `And` x)) `Or` (c `And` x)
   
va :: Formula a -> Formula a -> Formula a -> (Formula a, Formula a)                 
va x y c =
  (va_sum x y c, va_carry x y c)
     
-----------------------------
-- Addition von Binaerzahlen

type Varname = (String, Int)

x :: Int -> Formula Varname
x i = Var ("x", i)

y :: Int -> Formula Varname
y i = Var ("y", i)


-----------------------------
-- Ripple Carry Adder

rippleAdd :: Int -> ([Formula Varname], Formula Varname)
rippleAdd 0 = ([], FF)
rippleAdd size =
  let (sumbits, c) = rippleAdd (size - 1)
      (sum, carryout) = va (x (size - 1)) (y (size - 1)) c
  in (sumbits ++ [sum], carryout)

-----------------------------
-- Lookahead Carry Adder
     
gen :: Int -> Int -> Formula Varname
gen i j =
  if i == j then
    (x i) `And` (y i)
  else
    (gen k j) `Or` ((gen i (k - 1)) `And` (prop k j))
    where k = (j + 1 - i) `div` 2 + i 

prop :: Int -> Int -> Formula Varname
prop i j =
  if i == j then
    (x i) `Or` (y i)
  else
    (prop k j) `And` (prop i (k - 1))
    where k = (j + 1 - i) `div` 2 + i 

carry :: Int -> Formula Varname
carry 0 = FF
carry i = gen 0 (i - 1)
         
lookAheadAdd :: Int -> ([Formula Varname], Formula Varname)
lookAheadAdd 0 = ([], FF)
lookAheadAdd size =  
    let sumbits = [va_sum (x i) (y i) (carry i) | i <- [size - 1 .. 0]]
        c = va_carry (x (size - 1)) (y (size - 1)) (carry (size - 1))
    in (sumbits, c)

-- The following function generates a formula that says that both adders
-- produce the same output on *all* inputs.
equals :: Int -> Formula (String, Int)
equals size =
  let (sumbits1, carryout1) = rippleAdd size 
      (sumbits2, carryout2) = lookAheadAdd size
  in (carryout1 `Iff` carryout2) 
     `And`
     (BigAnd [s1 `Iff` s2 | (s1, s2) <- zip sumbits1 sumbits2])


main :: IO ()     
main =
  let size = 256 
  in do
    putStrLn $ "Teste Gleichheit fuer " ++ (show size) ++ " Bits."
    eta <- satisfiable (Neg (equals size))
    case eta of
      Unsatisfiable ->
        putStrLn "Lookahead- und Ripple-Addierer liefern gleiche Ergebnisse."
      Satisfiable eta -> do
        putStrLn $ "Lookahead- und Ripple-Addierer liefern "
          ++ "für folgende Eingabe verschiedene Ergebnisse: "
        putStrLn $ show eta
