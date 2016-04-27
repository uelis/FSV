package bdd;

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.List;
import net.sf.javabdd.*;

public class BDDOperations {

  /**
   * Das BDD, das die konstant wahre Funktion repraesentiert.
   */
  public static BDD constantTrue() {
    return B.one();    
  }

  /**
   * Das BDD, das die konstant falsche Funktion repraesentiert.
   */
  public static BDD constantFalse() {
    return B.zero();
  }

  /**
   * Das BDD fuer die Boolesche Funktion, deren Wert gerade gleich dem Wert der
   * Variablen v ist.
   */
  public static BDD var(BDDVar v) {
    return B.ithVar(v.num);
  }

  /**
   * Erzeuge eine neue Variable zur Benutzung in BDDs. Die Variablenreihenfolge
   * in den BDDs entspricht der Reihenfolge, in der diese Funktion aufgerufen
   * wird.
   */
  public static BDDVar freshBDDVar() {
    B.setVarNum(BDDVar.nextVar + 1);
    return BDDVar.fresh();
  }

  /**
   * Negation eines BDDs. Es wird ein neues BDD konstruiert, dass die Negation
   * von <code>b</code> repraesentiert. Das Objekt <code>b</code> wird dabei
   * nicht veraendert.
   */
  public static BDD neg(BDD b) {
    return b.not();
  }

  /**
   * Und-Verknuepfung zweier BDDs <code>b1</code> und <code>b2</code>. Der
   * Rueckgabewert dieser Funktion ist ein neues BDD, die Objekte
   * <code>b1</code> und <code>b2</code> werden nicht veraendert.
   */
  public static BDD and(BDD b1, BDD b2) {
    return b1.and(b2);
  }

  /**
   * Und-Verknuepfung einer Liste von BDDs. Der Rueckgabewert dieser Funktion
   * ist ein neues BDD, die Objekte in <code>bs</code> werden nicht veraendert.
   */
  public static BDD and(List<BDD> bs) {
    return and(bs.toArray(new BDD[bs.size()]));
  }

  /**
   * Und-Verknuepfung einer Liste von BDDs. Der Rueckgabewert dieser Funktion
   * ist ein neues BDD, die Objekte in <code>bs</code> werden nicht veraendert.
   */
  public static BDD and(BDD... bs) {
    BDD r = constantTrue();
    for (BDD b : bs) {
      r = and(r, b);
    }
    return r;
  }

  /**
   * Oder-Verknuepfung zweier BDDs <code>b1</code> und <code>b2</code>. Der
   * Rueckgabewert dieser Funktion ist ein neues BDD, die Objekte
   * <code>b1</code> und <code>b2</code> werden nicht veraendert.
   */
  public static BDD or(BDD b1, BDD b2) {
    return b1.or(b2);
  }

  /**
   * Oder-Verknuepfung einer Liste von BDDs. Der Rueckgabewert dieser Funktion
   * ist ein neues BDD, die Objekte in <code>bs</code> werden nicht veraendert.
   */
  public static BDD or(List<BDD> bs) {
    return or(bs.toArray(new BDD[bs.size()]));
  }

  /**
   * Oder-Verknuepfung einer Liste von BDDs. Der Rueckgabewert dieser Funktion
   * ist ein neues BDD, die Objekte in <code>bs</code> werden nicht veraendert.
   */
  public static BDD or(BDD... bs) {
    BDD r = constantFalse();
    for (BDD b : bs) {
      r = or(r, b);
    }
    return r;
  }

  /**
   * Genau-Dann-Wenn-Verknuepfung zweier BDDs <code>b1</code> und
   * <code>b2</code>. Der Rueckgabewert dieser Funktion ist ein neues BDD, die
   * Objekte <code>b1</code> und <code>b2</code> werden nicht veraendert.
   */
  public static BDD iff(BDD b1, BDD b2) {
    return and(or(neg(b1), b2), or(neg(b2), b1));
  }

  /**
   * Entweder-Oder-Verknuepfung zweier BDDs <code>b1</code> und <code>b2</code>.
   * Der Rueckgabewert dieser Funktion ist ein neues BDD, die Objekte
   * <code>b1</code> und <code>b2</code> werden nicht veraendert.
   */
  public static BDD xor(BDD b1, BDD b2) {
    return b1.xor(b2);
  }

  /**
   * Implikations-Verknuepfung zweier BDDs <code>b1</code> und <code>b2</code>.
   * Der Rueckgabewert dieser Funktion ist ein neues BDD, die Objekte
   * <code>b1</code> und <code>b2</code> werden nicht veraendert.
   */
  public static BDD imp(BDD b1, BDD b2) {
    return or(neg(b1), b2);
  }

  /**
   * Existenzielle Quantifizierung ueber eine Kollektion von Variablen in einem
   * BDD.
   *
   * <p>
   * Zum Beispiel entspricht <code>exist(Collections.singleton(v), b)</code> dem
   * BDD or(b[v:=0], b[v:=1]).
   *
   * <p>
   * Das Objekt <code>b</code> wird nicht veraendert.
   */
  public static BDD exist(Collection<BDDVar> vs, BDD b) {
    int[] ivs = new int[vs.size()];
    int i = 0;
    for (BDDVar v : vs) {
      ivs[i] = v.num;
      i++;
    }
    BDD exVars = B.makeSet(ivs);
    return b.exist(exVars);
  }

  /**
   * Umbenennung von Variablen im BDD <code>b</code>.
   *
   * <p>
   * Die Listen <code>oldVars</code> und <code>newVars</code> sollen disjunkte
   * Variablenlisten der gleichen Laenge sein. Besteht <code>oldVars</code> aus
   * o_1,o_2,...,o_k und <code>newVars</code> aus n_1,n_2,....n_k, so ist das
   * RueckgabeBDD von <code>replace(b, oldVars, newVars)</code> das BDD
   * <code>b[o_1:=n_1][o_2:=n_2]...[o_k:=n_k]</code>.
   *
   * <p>
   * Als Rueckgabewert wird ein neues Objekt erzeugt. Das Objekt <code>b</code>
   * wird nicht veraendert.
   */
  public static BDD replace(BDD b, List<BDDVar> oldVars, List<BDDVar> newVars) {
    if (oldVars.size() != newVars.size()) {
      throw new Error("Variablenlisten in replace muessen gleiche Laenge haben.");
    }
    // pruefe Disjunktheit:
    for (BDDVar v : oldVars) {
      if (newVars.contains(v)) {
        throw new Error("Variablenlisten sind nicht disjunkt.");
      }
    }
    BDDPairing pairing = B.makePair();
    for (int i = 0; i < oldVars.size(); i++) {
      pairing.set(oldVars.get(i).num, newVars.get(i).num);
    }
    return b.replace(pairing);
  }

  /**
   * Gleichheitstests fuer BDDs.
   */
  public static boolean equal(BDD b1, BDD b2) {
    return b1.equals(b2);
  }

  /**
   * Ausgabe des BDDs <code>b</code> in graphischer Form. Das BDD wird im
   * dot-Format ausgegeben und kann zum Beispiel mit dotty angezeigt werde
   * (siehe http://www.graphviz.com).
   *
   * <code>dotty dot_file.dot</code>
   */
  public static void printGraph(BDD b) {
    b.printDot();
  }

  /**
   * Ausgabe des BDDs <code>b</code> in graphischer Form. Das BDD wird im
   * dot-Format ausgegeben und kann zum Beispiel mit dotty angezeigt werde
   * (siehe http://www.graphviz.com).
   */
  public static void printGraph(PrintStream out, BDD b) {
    PrintStream oldOut = System.out;
    System.setOut(out);
    b.printDot();
    System.setOut(oldOut);
  }

  /**
   * Ausgabe des BDDs <code>b</code> in graphischer Form. Das BDD wird im
   * dot-Format ausgegeben und kann zum Beispiel mit dotty angezeigt werde
   * (siehe http://www.graphviz.com).
   *
   * Umwandlung einer dot-Datei in eine pdf-Datei mit dem Kommando dot:
   * {@code dot -Tpdf input.dot > output.pdf}
   */
  public static void printGraph(String fileName, BDD b) throws FileNotFoundException {
    printGraph(new PrintStream(fileName), b);
  }

  private static BDDFactory B = BDDFactory.init("j", 1000, 1000);
}
