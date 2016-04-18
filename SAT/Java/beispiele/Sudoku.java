package beispiele;

import cnf.Formula;

import static cnf.CNF.*;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.sat4j.specs.TimeoutException;

public class Sudoku {

  // Variablennamen
  private static String pos(int x, int y, int z) {
    return "pos(" + x + ", " + y + ", " + z + ")";
  }

  private static Formula sudoku(Integer[][] givens) {
    List<Formula> conditions = new LinkedList<>();

    // Vorgaben
    for (int x = 0; x < 9; x++) {
      for (int y = 0; y < 9; y++) {
        if (givens[x][y] != null) {
          conditions.add(var(pos(x, y, givens[x][y])));
        }
      }
    }

    // an jeder Stelle eine Zahl
    for (int x = 0; x < 9; x++) {
      for (int y = 0; y < 9; y++) {
        List<Formula> l = new LinkedList<>();
        for (int z = 1; z < 10; z++) {
          l.add(var(pos(x, y, z)));
        }
        conditions.add(or(l));
      }
    }

    // nicht zwei Zahlen an der gleichen Stelle
    for (int x = 0; x < 9; x++) {
      for (int y = 0; y < 9; y++) {
        for (int z1 = 1; z1 < 10; z1++) {
          for (int z2 = z1 + 1; z2 < 10; z2++) {
            conditions.add(or(neg(var(pos(x, y, z1))), neg(var(pos(x, y, z2)))));
          }
        }
      }
    }

    // jede Zahl hoechstens einmal in jeder Spalte
    for (int x = 0; x < 9; x++) {
      for (int y1 = 0; y1 < 9; y1++) {
        for (int y2 = y1 + 1; y2 < 9; y2++) {
          for (int z = 1; z < 10; z++) {
            conditions.add(or(neg(var(pos(x, y1, z))), neg(var(pos(x, y2, z)))));
          }
        }
      }
    }

    // jede Zahl hoechstens einmal in jeder Zeile
    for (int y = 0; y < 9; y++) {
      for (int x1 = 0; x1 < 9; x1++) {
        for (int x2 = x1 + 1; x2 < 9; x2++) {
          for (int z = 1; z < 10; z++) {
            conditions.add(or(neg(var(pos(x1, y, z))), neg(var(pos(x2, y, z)))));
          }
        }
      }
    }

    // keine Zahl in einem 3x3-Block doppelt
    for (int bx = 0; bx < 3; bx++) {
      for (int by = 0; by < 3; by++) {
        for (int x1 = 3 * bx; x1 < 3 * bx + 3; x1++) {
          for (int x2 = x1 + 1; x2 < 3 * bx + 3; x2++) {
            for (int y1 = 3 * by; y1 < 3 * by + 3; y1++) {
              for (int y2 = y1 + 1; y2 < 3 * by + 3; y2++) {
                for (int z = 1; z < 10; z++) {
                  conditions.add(or(neg(var(pos(x1, y1, z))), neg(var(pos(x2, y2, z)))));
                }
              }
            }
          }
        }
      }
    }

    return and(conditions);
  }

  public static void main(String[] args) throws TimeoutException {

    Integer b = null; // blank
    Integer[][] sudoku = {
      new Integer[]{6, b, b, b, 1, 7, 5, b, b},
      new Integer[]{b, 8, 1, 2, b, b, b, 7, b},
      new Integer[]{b, b, b, b, b, 5, b, b, b},
      new Integer[]{b, 2, 9, 4, b, b, b, b, 1},
      new Integer[]{b, 5, 4, b, 2, b, b, 3, b},
      new Integer[]{b, b, 6, b, 7, 8, b, 5, 4},
      new Integer[]{b, b, b, b, b, 9, 3, b, 7},
      new Integer[]{b, b, 3, 8, b, b, 4, b, b},
      new Integer[]{b, b, 5, b, b, b, b, 9, b}};

    Formula f = sudoku(sudoku);
    Map<Object, Boolean> trueVars = satisfiable(f);
    if (trueVars == null) {
      System.out.println("Nicht erfuellbar.");
    } else {
      // erfuellbar
      for (int x = 0; x < 9; x++) {
        for (int y = 0; y < 9; y++) {
          for (int z = 1; z < 10; z++) {
            if (trueVars.get(pos(x, y, z))) {
              System.out.print(z + " ");
            }
          }
        }
        System.out.println();
      }
    }
  }
}
