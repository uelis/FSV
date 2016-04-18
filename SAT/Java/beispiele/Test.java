package beispiele;

import cnf.Formula;

import java.io.IOException;
import java.util.Map;
import java.util.Set;
import static cnf.CNF.*;
import org.sat4j.specs.TimeoutException;

public class Test {

  public static void main(String[] args) throws TimeoutException, IOException {

    Formula f = or(and(var("x1"), var("x2")), and(var("x3"), var("x4")));

    System.out.println("Formel f: " + f);
    Map<Object, Boolean> eta = satisfiable(f);
    if (eta == null) {
      System.out.println("Formel f ist unerfüllbar.");
    } else {
      System.out.println("Formel f ist erfüllbar, "+
              " z.B. durch folgende Belegung");
      System.out.print(eta);
      System.out.println();
    }
    System.out.println("\n\nZu f erfuellbarkeitsaequivalente Formel in KNF im DIMACS-Format:");
    Map<Integer, Object> vars = writeDIMACS(System.out, f);
    System.out.println("Bedeutung der Variablen: " + vars);
  }
}
