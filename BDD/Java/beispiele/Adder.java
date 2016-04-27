package beispiele;

import static bdd.BDDOperations.*;
import bdd.BDDVar;
import net.sf.javabdd.BDD;
import java.util.List;
import java.util.LinkedList;

public class Adder {

    private static int size;
    
    private static List<BDDVar> xs;
    private static List<BDDVar> ys;
    
    private static BDD x(int i) {
        return var(xs.get(i));
    }
    
    private static BDD y(int i) {
        return var(ys.get(i));
    }

    private static BDD va_sum(BDD x, BDD y, BDD c) {
        return xor(xor(x, y), c);
    }
    
    private static BDD va_carry(BDD x, BDD y, BDD c) {
        return or(and(c, y), and(y, x), and(c, x));
    }

    private static BDD carry(int i) {
        if (i == 0) {
            return constantFalse();
        } else {
            return gen(0, i - 1);
        }
    }

    private static BDD gen(int i, int j) {
        if (i == j) {
            return and(x(i), y(i));
        } else {
            int k = (j + 1 - i) / 2 + i;
            return or(gen(k, j), and(gen(i, k - 1), prop(k, j)));
        }
    }
    
    private static BDD prop(int i, int j) {
        if (i == j) {
            return or(x(i), y(i));
        } else {
            int k = (j + 1 - i) / 2 + i;
            return and(prop(k, j), prop(i, k - 1));
        }
    }


  public static void main(String[] args) throws Exception {

      size = 300;
      
      xs = new LinkedList<>();
      ys = new LinkedList<>();
      for (int i = 0; i < size; i++) {
          xs.add(freshBDDVar());
          ys.add(freshBDDVar());
      }
      

      List<BDD> ripple_sumbits = new LinkedList<>();
      BDD ripple_carry = constantFalse();
      for (int i = 0; i < size; i++) {
          ripple_sumbits.add(va_sum(x(i), y(i), ripple_carry));
          ripple_carry = va_carry(x(i), y(i), ripple_carry);
      }
        
      List<BDD> lookahead_sumbits = new LinkedList<>();
      BDD lookahead_carry = constantFalse();
      for (int i = 0; i < size; i++) {
          lookahead_sumbits.add(va_sum(x(i), y(i), carry(i)));
      }

      for (int i = 0; i < size; i++) {
          if (equal(ripple_sumbits.get(i), lookahead_sumbits.get(i))) {
              System.out.println("Bit " + i + " ist korrekt");
          } else {
              System.out.println("Bit " + i + " ist nicht korrekt");
          }
      }
  }
}
