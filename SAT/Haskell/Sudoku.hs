module Main where

import Data.Map ((!))
import Formula
import Tseitin

sudoku :: Formula (Int, Int, Int)
sudoku = BigAnd [ at_least, one_number_each_cell, no_number_twice_in_row
                , no_number_twice_in_col, no_number_twice_in_square, givens ]
  where
     at_least =
       BigAnd [ BigOr [ Var (row, col, i) | i <- [1..9] ]
              | row <- [0..8],
                col <- [0..8] ]
     one_number_each_cell =
       BigAnd [ (Neg (Var (row, col, i))) `Or` (Neg (Var (row, col, j)))
              | row <- [0..8],
                col <- [0..8],
                i <- [1..9],
                j <- [1..9],
                i < j ]
     no_number_twice_in_row =
       BigAnd [ (Neg (Var (row, col1, i))) `Or` (Neg (Var (row, col2, i)))
              | row <- [0..8],
                col1 <- [0..8],
                col2 <- [0..8],
                i <- [1..9],
                col1 < col2 ]
     no_number_twice_in_col =
       BigAnd [ (Neg (Var (row1, col, i))) `Or` (Neg (Var (row2, col, i)))
              | row1 <- [0..8],
                row2 <- [0..8],
                col <- [0..8],
                i <- [1..9],
                row1 < row2 ]
     no_number_twice_in_square =
       BigAnd [ (Neg (Var (row1, col1, i))) `Or` (Neg (Var (row2, col2, i)))
              | x <- [0..2],
                y <- [0..2],
                row1 <- [3*x .. 3*x+2],
                row2 <- [3*x .. 3*x+2],
                col1 <- [3*y .. 3*y+2],
                col2 <- [3*y .. 3*y+2],
                i <- [1..9],
                col1 < col2 || row1 < row2 ]
     givens = BigAnd $ map Var
       [ (0,0,6), (0,4,1), (0,5,7), (0,6,5), (1,1,8), (1,2,1), (1,3,2), (1,7,7)
       , (2,5,5), (3,1,2), (3,2,9), (3,3,4), (3,8,1), (4,1,5), (4,2,4), (4,4,2)
       , (4,7,3), (5,2,6), (5,4,7), (5,5,8), (5,7,5), (5,8,4), (6,5,9), (6,6,3)
       , (6,8,7), (7,2,3), (7,3,8), (7,6,4), (8,2,5), (8,7,9)]

main :: IO ()     
main = do
  trueVars <- satisfiable sudoku
  case trueVars of
    Unsatisfiable ->
      putStr "unsatisfiable"
    Satisfiable eta -> do
      let char x y = concat [ show i | i <- [1..9], eta ! (y, x, i) ]
          line y = concat [ char x y | x <- [0..8] ]
          result = concat $ [ line y ++ "\n" | y <- [0..8] ]
      putStr result
