package aufgabe11_1;

import static cnf.CNF.*;
import cnf.Formula;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.sat4j.specs.TimeoutException;

public class Hamilton {

  private final Graph graph;
  private final int n;

  Hamilton(Graph graph) {
    this.graph = graph;
    this.n = graph.getNodeSet().size();
  }

  /**
   * Gibt den Namen der Variable q_{z,t} zurueck.
   */
  private String q(int z, int t) {
      return "q(" + z + ", " + t + ")";
  }

  /**
   * Erzeuge die Formel \bigand_{t=0}^{n-1} phi(t)
   */
  private Formula forall_times(Function<Integer, Formula> phi) {
    Formula r = tt();
    for (int t = 0; t < n; t++) {
      r = and(r, phi.apply(t));
    }
    return r;
  }

  /**
   * Erzeuge die Formel \bigand_{z ist Knoten} phi(z)
   */
  private Formula forall_nodes(Function<Integer, Formula> phi) {
    Formula r = tt();
    for (int n : graph.getNodeSet()) {
      r = and(r, phi.apply(n));
    }
    return r;
  }

  /**
   * Erzeuge die Formel \bigor_{z ist Knoten} phi(z)
   */
  private Formula exists_node(Function<Integer, Formula> phi) {
    Formula r = ff();
    for (int n : graph.getNodeSet()) {
      r = or(r, phi.apply(n));
    }
    return r;
  }

  private Formula hamiltonCircle() {
    Formula alwaysAtLeastOne
            = forall_times((t)
                    -> exists_node((z)
                            -> var(q(z, t))));
    Formula alwaysAtMostOneState
            = forall_times((t)
                    -> forall_nodes((z1)
                            -> forall_nodes((z2)
                                    -> (z1 < z2)
                                            ? neg(and(var(q(z1, t)), var(q(z2, t))))
                                            : tt())));
    Formula atMostOnce
            = forall_nodes((z)
                    -> forall_times((t1)
                            -> forall_times((t2)
                                    -> (t1 < t2)
                                            ? neg(and(var(q(z, t1)), var(q(z, t2))))
                                            : tt())));
    Formula transition
            = forall_times((t)
                    -> exists_node((z1)
                            -> exists_node((z2)
                                    -> graph.hasEdge(z1, z2)
                                            ? and(var(q(z1, t)), var(q(z2, (t + 1) % n)))
                                            : ff() )));
    return and(alwaysAtLeastOne, alwaysAtMostOneState, atMostOnce, transition);
  }

  public void findCycle() throws TimeoutException {
    Map<Object,Boolean> trueVars = satisfiable(hamiltonCircle());
    if (trueVars == null) {
      System.out.println("Der Graph hat keinen Hamiltonzyklus.");
    } else {
      System.out.println("Der Graph hat folgenden Hamiltonzyklus:");
      for (int t = 0; t < n; t++) {
        for (int z : graph.nodes) {
          if (trueVars.get(q(z, t))) {
            System.out.print(z);
          }
        }
        System.out.print((t < n - 1) ? " --> " : "\n");
      }
    }
  }

  public static void main(String[] args) throws TimeoutException {
// Generierung eines Zufallsgraphen mit bzw. ohne Hamiltonkreis
    Graph graph = Graph.makeRandomGraphWithHamiltonCircle(20);
//  Graph graph = Graph.makeRandomGraphWithoutHamiltonCircle(20);

    System.out.println("Eingabegraph:\n" + graph);
    Hamilton h = new Hamilton(graph);
    h.findCycle();
  }
}
