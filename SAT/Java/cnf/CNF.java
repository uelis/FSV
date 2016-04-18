package cnf;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.*;

/**
 * Methoden zur Konstruktion von aussagenlogischen
 * Formeln sowie zum Aufruf eines SAT-Solvers.
 * <p>
 * Diese Klasse ist das gesamte oeffentliche Interface des
 * Pakets {@code cnf}. Die anderen Klassen stellen kein
 * oeffentliches Interface bereit.
 */
public class CNF {

  // Diese Klasse sammelt nur statische Methoden; man benoetigt
  // keine Instanzen davon.
  private CNF() {
  }

  /**
   * Erzeugt die Formel, die durch eine aussagenlogische Variable
   * gegeben ist.
   *
   * @param name Name der Variable
   */
  public static Formula var(Object name) {
    if (name == null) {
      throw new NullPointerException("Variablenname darf nicht `null' sein!");
    }
    return new FormulaVar(name);
  }

  /**
   * Erzeugt die immer wahre Formel.
   */
  public static Formula tt() {
    return and();
  }

  /**
   * Erzeugt die immer falsche Formel.
   */
  public static Formula ff() {
    return or();
  }

  /**
   * Erzeugt die Negation einer gegbenen Formel.
   */
  public static Formula neg(Formula f) {
    return new FormulaNeg(f);
  }

  /**
   * Erzeugt die Konjunktion zweier Formeln.
   */
  public static Formula and(Formula f1, Formula f2) {
    List<Formula> fms = new LinkedList<>();
    fms.add(f1);
    fms.add(f2);
    return new FormulaAnd(fms);
  }

  /**
   * Erzeugt die Konjunktion einer Liste von Formeln.
   */
  public static Formula and(List<Formula> fms) {
    return new FormulaAnd(fms);
  }

  /**
   * Erzeugt die Konjunktion einer Liste von Formeln.
   */
  public static Formula and(Formula... fms) {
    return new FormulaAnd(Arrays.asList(fms));
  }

  /**
   * Erzeugt die Disjunktion zweier Formeln.
   */
  public static Formula or(Formula f1, Formula f2) {
    List<Formula> fms = new LinkedList<>();
    fms.add(f1);
    fms.add(f2);
    return new FormulaOr(fms);
  }

  /**
   * Erzeugt die Disjunktion einer Liste von Formeln.
   */
  public static Formula or(List<Formula> fms) {
    return new FormulaOr(fms);
  }

  /**
   * Erzeugt die Disjunktion einer Liste von Formeln.
   */
  public static Formula or(Formula... fms) {
    return new FormulaOr(Arrays.asList(fms));
  }

  /**
   * Erzeugt die Implikation zweier Formeln.
   *
   * @param fm1 Formel
   * @param fm2 Formel
   * @return Formel "fm1 => fm2"
   */
  public static Formula imp(Formula fm1, Formula fm2) {
    return or(neg(fm1), fm2);
  }

  /**
   * Erzeugt die Biimplikation zweier Formeln.
   *
   * @param fm1 Formel
   * @param fm2 Formel
   * @return Formel "fm1 <=> fm2"
   */
  public static Formula iff(Formula fm1, Formula fm2) {
    return and(imp(fm1, fm2), imp(fm2, fm1));
  }

  /**
   * Erzeugt das exklusive Oder zweier Formeln.
   */
  public static Formula xor(Formula fm1, Formula fm2) {
    return or(and(fm1, neg(fm2)), and(neg(fm1), fm2));
  }

  /**
   * Wendet die Tseitin-Transformation an und schreibt das Ergebnis im DIMACS-Format in den {@code os}.
   * Zurueckgegeben wird eine Map, welche die Zahlen im DIMACS-Text den Variablen in {@code f} zuordnet.
   */
  public static Map<Integer, Object> writeDIMACS(OutputStream os, Formula f) throws IOException {
    TseitinVisitor tseitinVisitor = new TseitinVisitor();
    Integer x = f.accept(tseitinVisitor);
    tseitinVisitor.writeResultDIMACS(os, x);
    return tseitinVisitor.getVarNameMap();
  }

  /**
   * Wandelt die uebergebene Formel in eine erfuellbarkeitsaequivalente Formel
   * in CNF um und ueberprueft sie mittels des SAT-Solvers SAT4j auf Erfuellbarkeit.
   * <p>
   * Zurueckgegeben wird eine erfuellende Belegung von {@ f} oder {@code null},
   * wenn es keine solche gibt.
   *
   * @param f Formel
   * @return Menge der Variablen, die in einer erfuellenden Belegung
   * von {@code f} wahr sind; oder {@code null} wenn {@code f}
   * unerfuellbar ist.
   * @throws TimeoutException
   */
  public static Map<Object, Boolean> satisfiable(Formula f) throws TimeoutException {
    TseitinVisitor tseitinVisitor = new TseitinVisitor();
    Integer x = f.accept(tseitinVisitor);
    Set<Set<Integer>> clauses = tseitinVisitor.getClauses();

    ISolver solver = SolverFactory.newDefault();

    solver.setExpectedNumberOfClauses(clauses.size());
    try {
      solver.addClause(new VecInt(new int[]{x}));
      for (Set<Integer> c : clauses) {
        int[] carr = new int[c.size()];
        int i = 0;
        for (Integer y : c) {
          carr[i] = y;
          i++;
        }
        solver.addClause(new VecInt(carr));
      }
    } catch (ContradictionException ex) {
      return null; // unsat
    }

    IProblem problem = solver;
    if (problem.isSatisfiable()) {
      int[] model = problem.model();
      Map<Object, Boolean> eta = new HashMap<>();
      for (Integer y : model) {
        Object var = tseitinVisitor.getVar(Math.abs(y));
        if (var != null) {
          eta.put(tseitinVisitor.getVar(Math.abs(y)), y > 0);
        }
      }
      return eta;
    } else {
      return null;
    }
  }
}
