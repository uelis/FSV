package beispiele;

import static bdd.BDDOperations.*;
import bdd.BDDVar;
import java.util.ArrayList;
import java.util.HashMap;
import net.sf.javabdd.BDD;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Peterson {

  private final int nproc = 5;
  private final int nlines = 5;

  // Alle erzeugten Variablen, nach ihrem Namen gespeichert
  private final Map<String, BDDVar> namedVars = new HashMap<>();

  private final List<BDDVar> vars = new ArrayList<>();
  private final List<BDDVar> vars1 = new ArrayList<>();

  Peterson() {

    // Erzeuge Variablen in der richtigen Reihenfolge
    for (int pid = 0; pid < nproc; pid++) {
      for (int line = 0; line < nlines; line++) {
        vars.add(lineVar(pid, line));
        vars1.add(lineVar1(pid, line));
      }
      for (boolean flag : new Boolean[]{true, false}) {
        vars.add(flagVar(pid, flag));
        vars1.add(flagVar1(pid, flag));
      }
      vars.add(turnVar(pid));
      vars1.add(turnVar1(pid));
    }
  }

  // Gibt die Variable mit Namen s zurueck. Wenn es noch keine
  // Variable mit diesem Namen gibt, dann wird eine neue angelegt.
  private BDDVar getNamedVar(String s) {
    BDDVar x = namedVars.get(s);
    if (x == null) {
      x = freshBDDVar();
      namedVars.put(s, x);
    }
    return x;
  }

  private BDDVar lineVar(int pid, int line) {
    return getNamedVar("line(" + pid + ", " + line + ")");
  }

  private BDDVar flagVar(int pid, boolean flag) {
    return getNamedVar("flag(" + pid + ", " + flag + ")");
  }

  private BDDVar turnVar(int pid) {
    return getNamedVar("turn(" + pid + ")");
  }

  private BDDVar lineVar1(int pid, int line) {
    return getNamedVar("line1(" + pid + ", " + line + ")");
  }

  private BDDVar flagVar1(int pid, boolean flag) {
    return getNamedVar("flag1(" + pid + ", " + flag + ")");
  }

  private BDDVar turnVar1(int pid) {
    return getNamedVar("turn1(" + pid + ")");
  }

  private int other(int pid) {
    return (pid + 1) % nproc;
  }

  private BDD sanity() {
    List<BDD> conditions = new LinkedList<>();

    // Jeder Prozess ist in einer Zeile
    for (int pid = 0; pid < nproc; pid++) {
      List<BDD> l = new LinkedList<>();
      for (int line = 0; line < nlines; line++) {
        l.add(var(lineVar(pid, line)));
      }
      conditions.add(or(l));
    }

    // Kein Prozess ist in zwei Zeilen gleichzeitig
    for (int pid = 0; pid < nproc; pid++) {
      for (int line1 = 0; line1 < nlines; line1++) {
        for (int line2 = line1 + 1; line2 < nlines; line2++) {
          conditions.add(
                  or(neg(var(lineVar(pid, line1))), neg(var(lineVar(pid, line2)))));
        }
      }
    }

    // Die Variable Turn ist ein Prozessindex
    List<BDD> l = new LinkedList<>();
    for (int pid = 0; pid < nproc; pid++) {
      l.add(var(turnVar(pid)));
    }
    conditions.add(or(l));

    // Die Variable Turn ist genau ein Prozessindex
    for (int pid1 = 0; pid1 < nproc; pid1++) {
      for (int pid2 = pid1 + 1; pid2 < nproc; pid2++) {
        conditions.add(or(neg(var(turnVar(pid1))), neg(var(turnVar(pid2)))));
      }
    }

    // Jedes Flag hat genau einen Wert
    for (int pid = 0; pid < nproc; pid++) {
      conditions.add(iff(var(flagVar(pid, false)), neg(var(flagVar(pid, true)))));
    }

    return and(conditions);
  }

  private BDD initial() {
    List<BDD> conditions = new LinkedList<>();

    conditions.add(var(turnVar(0)));
    for (int pid = 0; pid < nproc; pid++) {
      conditions.add(var(flagVar(pid, false)));
      conditions.add(var(lineVar(pid, 0)));
    }
    return and(conditions);
  }

  private BDD transition() {
    List<BDD> conditions = new LinkedList<>();

    List<BDD> cases = new LinkedList<>();
    for (int pid = 0; pid < nproc; pid++) {
      // Moeglichkeit: Prozess pid macht einen Schritt.
      List<BDD> pidMakesMove = new LinkedList<>();

      // andere Prozesse machen nichts
      for (int pid2 = 0; pid2 < nproc; pid2++) {
        if (pid2 == pid) {
          continue;
        }
        pidMakesMove.add(iff(var(flagVar(pid2, false)), var(flagVar1(pid2, false))));
        pidMakesMove.add(iff(var(flagVar(pid2, true)), var(flagVar1(pid2, true))));
        for (int line = 0; line < nlines; line++) {
          pidMakesMove.add(iff(var(lineVar(pid2, line)), var(lineVar1(pid2, line))));
        }
      }

      // Prozess pid macht einen Schritt.
      // Alle Moeglichkeiten sind hier aufgezaehlt
      List<BDD> pidPossibilitiesForMove = new LinkedList<>();
      for (boolean flag : new Boolean[]{false, true}) {
        for (boolean flagOther : new Boolean[]{false, true}) {
          for (boolean flag1 : new Boolean[]{false, true}) {
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
                              and(var(flagVar(pid, flag)),
                                      var(flagVar(other(pid), flagOther)),
                                      var(turnVar(turn)),
                                      var(lineVar(pid, line)),
                                      var(flagVar1(pid, flag1)),
                                      var(turnVar1(turn1)),
                                      var(lineVar1(pid, line1))));
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
    return and(conditions);
  }

  private BDD undesired() {
    List<BDD> undesired = new LinkedList<>();
    for (int pid1 = 0; pid1 < nproc; pid1++) {
      for (int pid2 = pid1 + 1; pid2 < nproc; pid2++) {
        undesired.add(and(var(lineVar(pid1, 4)), var(lineVar(pid2, 4))));
      }
    }
    return or(undesired);
  }

  public boolean isOk() {
    final BDD init = initial();
    final BDD sanity = sanity();
    final BDD trans = transition();
    final BDD undes = undesired();

    BDD reachable = and(init, sanity);
    while (true) {
      BDD b0 = reachable;
      BDD b1 = and(reachable, trans);
      BDD b2 = exist(vars, b1);
      BDD b3 = replace(b2, vars1, vars);
      BDD b4 = and(b3, sanity);
      reachable = or(reachable, b4);
      if (equal(reachable, b0)) {
        break;
      }
    }

    System.out.println(reachable.satCount());
    
    return equal(constantFalse(), and(undes, reachable));
  }

  public static void main(String[] args) {

    Peterson p = new Peterson();

    if (p.isOk()) {
      System.out.println("Wechselseitiger Ausschluss ist gegeben.");
    } else {
      System.out.println("Wechselseitiger Ausschluss ist nicht gegeben.");
    }
  }
}
