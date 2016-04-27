package beispiele;

import static bdd.BDDOperations.*;
import bdd.BDDVar;
import java.util.ArrayList;
import java.util.HashMap;
import net.sf.javabdd.BDD;
import java.util.List;
import java.util.Map;

public class Semaphore {

  private final int nproc = 10;

  private enum ProcessState {

    SLEEP, WAIT, WORK
  };

  private enum SemaphoreState {

    FREE, OCC
  };

  // Alle erzeugten Variablen, nach ihrem Namen gespeichert
  private final Map<String, BDDVar> namedVars = new HashMap<>();

  private final List<BDDVar> vars = new ArrayList<>();
  private final List<BDDVar> vars1 = new ArrayList<>();

  Semaphore() {

    // Erzeuge Variablen in der richtigen Reihenfolge
    for (int pid = 0; pid < nproc; pid++) {
      for (ProcessState z : ProcessState.values()) {
        vars.add(procVar(pid, z));
        vars1.add(procVar1(pid, z));
      }
    }
    for (SemaphoreState w : SemaphoreState.values()) {
      vars.add(semVar(w));
      vars1.add(semVar1(w));
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

  private BDDVar procVar(int pid, ProcessState z) {
    return getNamedVar("proc(" + pid + ", " + z + ")");
  }

  private BDDVar semVar(SemaphoreState s) {
    return getNamedVar("sema(" + s + ")");
  }

  private BDDVar procVar1(int pid, ProcessState z) {
    return getNamedVar("proc1(" + pid + ", " + z + ")");
  }

  private BDDVar semVar1(SemaphoreState s) {
    return getNamedVar("sema1(" + s + ")");
  }

  private BDD sanity() {
    BDD sanity = constantTrue();

    // Die Semaphore ist in einem Zustand
    sanity = and(sanity,
            or(var(semVar(SemaphoreState.FREE)), var(semVar(SemaphoreState.OCC))));

    // Die Semaphore ist nicht gleichzeitig in zwei Zustaenden
    sanity = and(sanity,
            or(neg(var(semVar(SemaphoreState.FREE))), neg(var(semVar(SemaphoreState.OCC)))));

    // Jeder Prozess ist in einem Zustand
    for (int pid = 0; pid < nproc; pid++) {
      BDD one_state = constantFalse();
      for (ProcessState z : ProcessState.values()) {
        one_state = or(one_state, var(procVar(pid, z)));
      }
      sanity = and(sanity, one_state);
    }

    // Kein Prozess ist in zwei Zustaenden gleichzeitig
    for (int pid = 0; pid < nproc; pid++) {
      for (ProcessState z1 : ProcessState.values()) {
        for (ProcessState z2 : ProcessState.values()) {
          if (!z1.equals(z2)) {
            sanity = and(sanity,
                    or(neg(var(procVar(pid, z1))), neg(var(procVar(pid, z2)))));
          }
        }
      }
    }

    return and(sanity);
  }

  private BDD initial() {
    BDD initial = sanity();

    initial = and(initial, var(semVar(SemaphoreState.FREE)));

    for (int p = 0; p < nproc; p++) {
      initial = and(initial, var(procVar(p, ProcessState.SLEEP)));
    }

    return initial;
  }

  private BDD transition() {
    BDD trans = constantFalse();

    for (int pid = 0; pid < nproc; pid++) {
      // andere Prozesse machen nichts
      BDD othersDoNotMove = constantTrue();
      for (int pid2 = 0; pid2 < nproc; pid2++) {
        if (pid2 == pid) {
          continue;
        }
        for (ProcessState z : ProcessState.values()) {
          othersDoNotMove = and(othersDoNotMove,
                  iff(var(procVar(pid2, z)), var(procVar1(pid2, z))));
        }
      }

      // Prozess pid macht einen Schritt.
      // Alle Moeglichkeiten sind hier aufgezaehlt
      BDD possibilitiesForMove = 
              or(
              and(var(procVar(pid, ProcessState.SLEEP)), var(semVar(SemaphoreState.FREE)), 
                      var(procVar1(pid, ProcessState.WAIT)), var(semVar1(SemaphoreState.FREE))),
              and(var(procVar(pid, ProcessState.SLEEP)), var(semVar(SemaphoreState.OCC)), 
                      var(procVar1(pid, ProcessState.WAIT)), var(semVar1(SemaphoreState.OCC))),
              and(var(procVar(pid, ProcessState.WAIT)), var(semVar(SemaphoreState.FREE)), 
                      var(procVar1(pid, ProcessState.WORK)), var(semVar1(SemaphoreState.OCC))),
              and(var(procVar(pid, ProcessState.WORK)), 
                      var(procVar1(pid, ProcessState.SLEEP)), var(semVar1(SemaphoreState.FREE)))
              );
      
      trans = or(trans, and(othersDoNotMove, possibilitiesForMove));
    }
    return trans;
  }

  private BDD undesired() {
    BDD undesired = constantFalse();

    for (int pid1 = 0; pid1 < nproc; pid1++) {
      for (int pid2 = pid1 + 1; pid2 < nproc; pid2++) {
        undesired = or(undesired,
                and(var(procVar(pid1, ProcessState.WORK)), var(procVar(pid2, ProcessState.WORK))));
      }
    }
    return undesired;
  }

  public boolean isOk() {
    final BDD sanity = sanity();
    final BDD trans = transition();
    final BDD undes = undesired();

    BDD reachable = initial();
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

    return equal(constantFalse(), and(undes, reachable));
  }

  public static void main(String[] args) {

    Semaphore p = new Semaphore();

    if (p.isOk()) {
      System.out.println("Wechselseitiger Ausschluss ist gegeben.");
    } else {
      System.out.println("Wechselseitiger Ausschluss ist nicht gegeben.");
    }
  }
}
