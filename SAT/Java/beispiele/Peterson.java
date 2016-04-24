package beispiele;

import cnf.Formula;

import static cnf.CNF.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.sat4j.specs.TimeoutException;

public class Peterson {

  private static final int tmax = 20;
  private static final int nproc = 2;
  private static final int nlines = 5;

  private static String lineVar(int t, int pid, int line) {
    return ("line(" + t + ", " + pid + ", " + line + ")");
  }

  private static String flagVar(int t, int pid, boolean flag) {
    return ("flag(" + t + ", " + pid + ", " + flag + ")");
  }

  private static String turnVar(int t, int pid) {
    return ("turn(" + t + ", " + pid + ")");
  }

  private static int other(int pid) {
    return (pid + 1) % nproc;
  }

  private static Formula sanity() {
    List<Formula> conditions = new LinkedList<>();

    // Jeder Prozess ist in einer Zeile
    for (int t = 0; t < tmax; t++) {
      for (int pid = 0; pid < nproc; pid++) {
        List<Formula> l = new LinkedList<>();
        for (int line = 0; line < nlines; line++) {
          l.add(var(lineVar(t, pid, line)));
        }
        conditions.add(or(l));
      }
    }

    // Kein Prozess ist in zwei Zeilen gleichzeitig
    for (int t = 0; t < tmax; t++) {
      for (int pid = 0; pid < nproc; pid++) {
        for (int line1 = 0; line1 < nlines; line1++) {
          for (int line2 = line1 + 1; line2 < nlines; line2++) {
            conditions.add(
                    or(neg(var(lineVar(t, pid, line1))), neg(var(lineVar(t, pid, line2)))));
          }
        }
      }
    }

    // Die Variable Turn ist ein Prozessindex
    for (int t = 0; t < tmax; t++) {
      List<Formula> l = new LinkedList<>();
      for (int pid = 0; pid < nproc; pid++) {
        l.add(var(turnVar(t, pid)));
      }
      conditions.add(or(l));
    }

    // Die Variable Turn ist genau ein Prozessindex
    for (int t = 0; t < tmax; t++) {
      for (int pid1 = 0; pid1 < nproc; pid1++) {
        for (int pid2 = pid1 + 1; pid2 < nproc; pid2++) {
          conditions.add(or(neg(var(turnVar(t, pid1))), neg(var(turnVar(t, pid2)))));
        }
      }
    }

    // Jedes Flag hat genau einen Wert
    for (int t = 0; t < tmax; t++) {
      for (int pid = 0; pid < nproc; pid++) {
        conditions.add(iff(var(flagVar(t, pid, false)), neg(var(flagVar(t, pid, true)))));
      }
    }

    return and(conditions);
  }

  private static Formula initial() {
    List<Formula> conditions = new LinkedList<>();

    conditions.add(var(turnVar(0, 0)));
    for (int pid = 0; pid < nproc; pid++) {
      conditions.add(var(flagVar(0, pid, false)));
      conditions.add(var(lineVar(0, pid, 0)));
    }
    return and(conditions);
  }

  private static Formula transition() {
    List<Formula> conditions = new LinkedList<>();

    for (int t = 0; t < tmax - 1; t++) {
      List<Formula> cases = new LinkedList<>();
      for (int pid = 0; pid < nproc; pid++) {
        // Moeglichkeit: Prozess pid macht einen Schritt.
        List<Formula> pidMakesMove = new LinkedList<>();

        // andere Prozesse machen nichts
        for (int pid2 = 0; pid2 < nproc; pid2++) {
          if (pid2 == pid) {
            continue;
          }
          pidMakesMove.add(iff(var(flagVar(t, pid2, false)), var(flagVar(t + 1, pid2, false))));
          pidMakesMove.add(iff(var(flagVar(t, pid2, true)), var(flagVar(t + 1, pid2, true))));
          for (int line = 0; line < nlines; line++) {
            pidMakesMove.add(iff(var(lineVar(t, pid2, line)), var(lineVar(t + 1, pid2, line))));
          }
        }

        // Prozess pid macht einen Schritt.
        // Alle Moeglichkeiten sind hier aufgezaehlt
        List<Formula> pidPossibilitiesForMove = new LinkedList<>();
        for (Boolean flag : new Boolean[]{false, true}) {
          for (Boolean flagOther : new Boolean[]{false, true}) {
            for (Boolean flag1 : new Boolean[]{false, true}) {
              for (int turn = 0; turn < nproc; turn++) {
                for (int turn1 = 0; turn1 < nproc; turn1++) {
                  for (int line = 0; line < nlines; line++) {
                    for (int line1 = 0; line1 < nlines; line1++) {
                      if ((line == 0 && line1 == 1 && turn1 == turn && flag1 == true)
                              || (line == 1 && line1 == 2 && turn1 == other(pid) && flag1 == flag)
                              || (line == 2 && flagOther == true && line1 == 3 && turn1 == turn && flag1 == flag)
                              || (line == 2 && flagOther == false && line1 == 4 && turn1 == turn && flag1 == flag)
                              || (line == 3 && turn != pid && line1 == 2 && turn1 == turn && flag1 == flag)
                              || (line == 3 && turn == pid && line1 == 4 && turn1 == turn && flag1 == flag)
                              || (line == 4 && line1 == 0 && turn1 == turn && flag1 == false)) {
                        pidPossibilitiesForMove.add(
                                and(var(flagVar(t, pid, flag)),
                                        var(flagVar(t, other(pid), flagOther)),
                                        var(turnVar(t, turn)),
                                        var(lineVar(t, pid, line)),
                                        var(flagVar(t + 1, pid, flag1)),
                                        var(turnVar(t + 1, turn1)),
                                        var(lineVar(t + 1, pid, line1))));
                      }
                    }
                  }
                }
              }
            }
          }
        }
        pidMakesMove.add(or(pidPossibilitiesForMove));
        cases.add(and(pidMakesMove));
      }
      conditions.add(or(cases));
    }
    return and(conditions);
  }

  private static Formula undesired() {
    List<Formula> undesired = new LinkedList<>();
    for (int t = 0; t < tmax; t++) {
      for (int pid1 = 0; pid1 < nproc; pid1++) {
        for (int pid2 = pid1 + 1; pid2 < nproc; pid2++) {
          undesired.add(and(var(lineVar(t, pid1, 4)), var(lineVar(t, pid2, 4))));
        }
      }
    }
    return or(undesired);
  }

  private static Formula peterson() {
    return and(sanity(), initial(), transition(), undesired());
  }

  public static void main(String[] args) throws TimeoutException {

    Formula f = peterson();
    Map<Object, Boolean> eta = satisfiable(f);
    if (eta == null) {
      System.out.println("Nicht erfuellbar.");
    } else {
      // erfuellbar 

      System.out.println("Wahre Variablen einer erfuellenden Belegung: ");
      for (Map.Entry<Object, Boolean> e : eta.entrySet()) {
        if (e.getValue()) {
          System.out.println(e.getKey());
        }
      }

      System.out.println("\nBedeutung:");
      for (int t = 0; t < tmax; t++) {
        System.out.println("Zeit: " + t);
        for (int pid = 0; pid < nproc; pid++) {
          if (eta.get(turnVar(t, pid))) {
            System.out.println("turn = " + pid);
          }
        }
        for (int pid = 0; pid < nproc; pid++) {
          System.out.print("Prozess: " + pid);
          for (int line = 0; line < nlines; line++) {
            if (eta.get(lineVar(t, pid, line))) {
              System.out.print(", Zeile: " + line);
            }
          }
          for (Boolean flag : new Boolean[]{false, true}) {
            if (eta.get(flagVar(t, pid, flag))) {
              System.out.print(", flag[" + pid + "] = " + flag);
            }
          }
          System.out.println();
        }
        System.out.println("==");
      }
    }
  }
}
