package beispiele;

import cnf.Formula;
import org.sat4j.specs.TimeoutException;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static cnf.CNF.*;

public class Damen {

  private static final int n = 8;

  // Variablennamen
  private static String dame(int x, int y) {
    return "dame(" + x + ", " + y + ")";
  }

  private static Formula damen() {
    List<Formula> conditions = new LinkedList<>();

    // in jeder Spalte eine Dame
    for (int x = 0; x < n; x++) {
      List<Formula> l = new LinkedList<>();
      for (int y = 0; y < n; y++) {
        l.add(var(dame(x, y)));
      }
      conditions.add(or(l));
    }

    // jede Dame hoechstens einmal in jeder Spalte
    for (int x = 0; x < n; x++) {
      for (int y1 = 0; y1 < n; y1++) {
        for (int y2 = y1 + 1; y2 < n; y2++) {
          conditions.add(or(neg(var(dame(x, y1))), neg(var(dame(x, y2)))));
        }
      }
    }

    // jede Dame hoechstens einmal in jeder Zeile
    for (int y = 0; y < n; y++) {
      for (int x1 = 0; x1 < n; x1++) {
        for (int x2 = x1 + 1; x2 < n; x2++) {
          conditions.add(or(neg(var(dame(x1, y))), neg(var(dame(x2, y)))));
        }
      }
    }

    // Damen duerfen nicht diagonal stehen
    for (int x1 = 0; x1 < n; x1++) {
      for (int y1 = 0; y1 < n; y1++) {
        for (int x2 = x1 + 1; x2 < n; x2++) {
          for (int y2 = 0; y2 < n; y2++) {
            if (x2 - x1 == Math.abs(y2 - y1))  {
              conditions.add(or(neg(var(dame(x1, y1))), neg(var(dame(x2, y2)))));
            }
          }
        }
      }
    }

    return and(conditions);
  }

  public static void main(String[] args) throws TimeoutException {

    Map<Object, Boolean> eta = satisfiable(damen());
    if (eta == null) {
      System.out.println("Nicht erfuellbar.");
    } else {
      // erfuellbar
      for (int x = 0; x < n; x++) {
        for (int y = 0; y < n; y++) {
          System.out.print(eta.get(dame(x, y)) ? "D " : ". ");
        }
        System.out.println();
      }
    }
  }
}
