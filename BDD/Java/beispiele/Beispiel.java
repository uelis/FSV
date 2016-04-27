package beispiele;

import static bdd.BDDOperations.*;
import bdd.BDDVar;
import net.sf.javabdd.BDD;

public class Beispiel {

  public static void main(String[] args) throws Exception {
    // Erzeuge Variablen in der Reihenfolge ihrer Ordnung
    BDDVar vx = freshBDDVar();
    BDDVar vy = freshBDDVar();
    BDDVar vc = freshBDDVar();

    BDD x = var(vx);
    BDD y = var(vy);
    BDD c = var(vc);

    // Verknuepfung von BDDs
    BDD b = or(and(var(x), var(y)), and(var(x), var(c)), and(var(y), var(c)));

    BDD b1 = or(var(x), var(y), var(c));
    System.out.println("b und b1 sind gleich: " + equal(b, b1));

    BDD b2 = or(and(var(x), or(var(y), var(c))), and(var(y), var(c)));
    System.out.println("b und b2 sind gleich: " + equal(b, b2));

    // Beispiel fuer Gleichheitstest
    boolean sat = equal(neg(b), constantFalse());
    System.out.println("b ist " + (sat ? "erfuellbar" : "unerfuellbar"));

    // Ausgabe als graph
    printGraph("b.dot", b);
    System.out.println("Das BDD fuer b wurde in Datei b.dot geschrieben.");
  }
}
