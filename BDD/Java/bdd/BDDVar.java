package bdd;

/**
 * Eine Klasse zur Repraesetierung von Variablen in BDDs.
 * Variablen koennen mittels BDDOperations.freshBDDVar erzeugt werden.
 *
 */
public class BDDVar {

  final int num;

  private BDDVar(int num) {
    this.num = num;
  }

  static BDDVar fresh() {
    return new BDDVar(nextVar++);
  }
  
  static int nextVar = 0;

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final BDDVar other = (BDDVar) obj;
    if (this.num != other.num) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 29 * hash + this.num;
    return hash;
  }
}
