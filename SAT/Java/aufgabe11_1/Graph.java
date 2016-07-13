package aufgabe11_1;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;

public class Graph {

  final Set<Integer> nodes;
  final Map<Integer, Set<Integer>> successors;
  final Map<Integer, String> colors;

  private Graph() {
    nodes = new HashSet<>();
    successors = new HashMap<>();
    colors = new HashMap<>();
  }

  /**
   * Gibt die Menge der Knoten zurueck. Die Knoten selbst sind hier Integer.
   * @param size
   * @return 
   */
  Set<Integer> getNodeSet() {
    return Collections.unmodifiableSet(nodes);
  }

  /**
   * Gibt {@code true} zurueck falls der Graph eine Kante von {@code from} nach {@code to} hat.
   * @param size
   * @return 
   */
  boolean hasEdge(int from, int to) {
    Set<Integer> succs = successors.get(from);
    return (succs != null) && (succs.contains(to));
  }
  
  void setColor(int node, String color) {
    colors.put(node, color);
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("digraph G {\n");
    for (int m : nodes) {
      if (colors.get(m) != null) {
        builder.append(m);
        builder.append("[style=filled, fillcolor=");
        builder.append(colors.get(m));
        builder.append("];\n");
      }
      for (int n : nodes) {
        if (hasEdge(m, n)) {
          builder.append(m).append(" -> ").append(n).append(";\n");
        }
      }
    }
      builder.append("}");
    return builder.toString();
  }

  /**
   * Erzeugt einen zufaelligen Graphen der gegebenen Groesse, der einen Hamiltonkreis enthaelt.
   * @param size
   * @return 
   */
  static Graph makeRandomGraphWithHamiltonCircle(int size) {

    Graph g = new Graph();
    ArrayList<Integer> l = new ArrayList();

    for (int i = 0; i < size; i++) {
      g.nodes.add(i);
      g.successors.put(i, new HashSet<>());
      l.add(i);
    }

    Collections.shuffle(l);
    for (int i = 0; i < size; i++) {
      g.successors.get(l.get(i)).add(l.get((i + 1) % size));
    }

    Random r = new Random();
    for (int i = 0; i < size; i++) {
      for (int j = 0; j < size; j++) {
        if (r.nextDouble() < 0.1) {
          g.successors.get(i).add(j);
        }
      }
    }
    return g;
  }

  /**
   * Erzeugt einen zufaelligen Graphen der gegebenen Groesse, der keinen Hamiltonkreis enthaelt.
   * @param size
   * @return 
   */
  static Graph makeRandomGraphWithoutHamiltonCircle(int size) {
    Graph g = new Graph();
    for (int i = 0; i < size; i++) {
      g.nodes.add(i);
      g.successors.put(i, new HashSet<>());
    }

    Graph g1 = makeRandomGraphWithHamiltonCircle(size / 2);
    Graph g2 = makeRandomGraphWithHamiltonCircle(size / 2 + size % 2);
    for (int m : g1.nodes) {
      for (int n : g1.nodes) {
        if (g1.hasEdge(m, n)) {
          g.successors.get(m).add(n);
        }
      }
    }

    for (int m : g2.nodes) {
      for (int n : g2.nodes) {
        if (g2.hasEdge(m, n)) {
          g.successors.get(m + g1.nodes.size()).add(n + g1.nodes.size());
        }
      }
    }

    Random r = new Random();
    int m = r.nextInt(g1.nodes.size());
    int n = r.nextInt(g2.nodes.size());
    g.successors.get(m).add(n + g1.nodes.size());

    return g;

  }
}
