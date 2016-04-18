package cnf;

import java.io.*;
import java.util.*;

import static cnf.CNF.*;

class TseitinVisitor implements FormulaVisitor<Integer> {

  private int nextId;
  private Map<Formula, Integer> fmVars;
  private Map<Object, Integer> ids;
  private Map<Integer, Object> vars;
  private Set<Set<Integer>> clauses;

  TseitinVisitor() {
    nextId = 1;
    fmVars = new HashMap<>();
    ids = new HashMap<>();
    vars = new HashMap<>();
    clauses = new HashSet<>();
  }

  Set<Set<Integer>> getClauses() {
    return clauses;
  }

  Formula getResultFormula(Integer x) {
    List<Formula> clfms = new LinkedList<>();
    clfms.add(var(x));
    for (Set<Integer> c : clauses) {
      List<Formula> lits = new LinkedList<>();
      for (Integer y : c) {
        Object v = getVar(y);
        if (v == null) {
          v = "fresh" + Math.abs(y);
        }
        if (y > 0) {
          lits.add(var(v));
        } else {
          lits.add(neg(var(v)));
        }
      }
      clfms.add(or(lits));
    }
    return and(clfms);
  }

  void writeResultDIMACS(OutputStream os, Integer x) throws IOException {
    BufferedWriter w = new BufferedWriter(new OutputStreamWriter(os));
    w.write("fm " + (clauses.size() + 1) + " " + nextId + "\n");
    w.write(x + " 0\n");
    for (Set<Integer> c : clauses) {
      for (Integer y : c) {
        w.write(y.toString());
        w.write(" ");
      }
      w.write("0\n");
    }
    w.flush();
  }

  private int freshName() {
    return nextId++;
  }

  int getId(Object o) {
    Integer x = ids.get(o);
    if (x == null) {
      x = freshName();
      ids.put(o, x);
      vars.put(x, o);
    }
    return x;
  }

  Object getVar(int x) {
    return vars.get(x);
  }

  Map<Integer, Object> getVarNameMap() {
    return Collections.unmodifiableMap(vars);
  }

  public Integer visitVar(FormulaVar fm) {
    int x = getId(fm.name);
    fmVars.put(fm, x);
    return x;
  }

  public Integer visitNeg(FormulaNeg fm) {
    Integer xbody = fmVars.get(fm.fm);
    if (xbody == null) {
      xbody = fm.fm.accept(this);
    }
    Integer x = freshName();
    fmVars.put(fm, x);
    Set<Integer> clause = new TreeSet<Integer>();
    clause.add(x);
    clause.add(xbody);
    clauses.add(clause);
    clause = new TreeSet<Integer>();
    clause.add(-x);
    clause.add(-xbody);
    clauses.add(clause);
    return x;
  }

  public Integer visitOr(FormulaOr fm) {
    List<Integer> xs = new LinkedList<Integer>();
    for (Formula f : fm.fms) {
      Integer x = fmVars.get(f);
      if (x == null) {
        x = f.accept(this);
      }
      xs.add(x);
    }
    Integer x = freshName();
    fmVars.put(fm, x);

    Set<Integer> clause = new TreeSet<Integer>();
    clause.add(-x);
    clause.addAll(xs);
    clauses.add(clause);

    for (Integer y : xs) {
      clause = new TreeSet<Integer>();
      clause.add(x);
      clause.add(-y);
      clauses.add(clause);
    }

    return x;
  }

  public Integer visitAnd(FormulaAnd fm) {
    List<Integer> xs = new LinkedList<Integer>();
    for (Formula f : fm.fms) {
      Integer x = fmVars.get(f);
      if (x == null) {
        x = f.accept(this);
      }
      xs.add(x);
    }
    Integer x = freshName();
    fmVars.put(fm, x);

    Set<Integer> clause = new TreeSet<Integer>();
    clause.add(x);
    for (Integer y : xs) {
      clause.add(-y);
    }
    clauses.add(clause);

    for (Integer y : xs) {
      clause = new TreeSet<Integer>();
      clause.add(-x);
      clause.add(y);
      clauses.add(clause);
    }

    return x;
  }
}

