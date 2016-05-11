package beispiele;

import cnf.Formula;

import static cnf.CNF.*;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import static beispiele.BauerHundKatzeMaus.Person.*;

import org.sat4j.specs.TimeoutException;

public class BauerHundKatzeMaus {

    enum Person {
        BAUER, HUND, KATZE, MAUS
    };

    private static String links(Person pers, int t) {
        return "links(" + pers + "," + t + ")";
    }

    private static String rechts(Person pers, int t) {
        return "rechts(" + pers + "," + t + ")";
    }

    private static Formula bauerHundKatzeMaus(int maxZeit) {

        List<Formula> conditions = new LinkedList<Formula>();

        // Am Anfang alle links
        for (Person p : Person.values()) {
            conditions.add(var(links(p, 0)));
        }

        // Am Ende alle rechts
        for (Person p : Person.values()) {
            conditions.add(var(rechts(p, maxZeit)));
        }

        // niemand ist sowohl links als auch rechts,
        // jeder ist entweder links oder rechts
        for (int t = 0; t <= maxZeit; t++) {
            for (Person p : Person.values()) {
                conditions.add(neg(and(var(links(p, t)), var(rechts(p, t)))));
                conditions.add(or(var(links(p, t)), var(rechts(p, t))));
            }
        }

        // niemals Ziege und Kohl ohne Hirte und niemals Wolf und Ziege ohne Hirte
        for (int t = 0; t <= maxZeit; t++) {
            conditions.add(imp(and(var(links(KATZE, t)), var(links(MAUS, t))), var(links(BAUER, t))));
            conditions.add(imp(and(var(rechts(KATZE, t)), var(rechts(MAUS, t))), var(rechts(BAUER, t))));
            conditions.add(imp(and(var(links(KATZE, t)), var(links(HUND, t))), var(links(BAUER, t))));
            conditions.add(imp(and(var(rechts(KATZE, t)), var(rechts(HUND, t))), var(rechts(BAUER, t))));
        }

        // Uebergangsmoeglichkteiten
        for (int t = 0; t < maxZeit; t++) {

            List<Formula> moeglichkeiten = new LinkedList<Formula>();

            // 1. Moeglichkeit: nichts aendert sich
            List<Formula> l = new LinkedList<>();
            for (Person p : Person.values()) {
                l.add(and(iff(var(links(p, t)), var(links(p, t + 1))),
                          iff(var(rechts(p, t)), var(rechts(p, t + 1)))));
            }
            moeglichkeiten.add(and(l));

            // 2. Möglichkeit: Der Bauer setzt über den Fluss und nimmt eine Person
            //                 (evtl. er selbst) mit.
            for (Person i : Person.values()) {
                l = new LinkedList<>();
                // Person i ist vorher auf der gleichen Seite wie der Bauer:
                l.add(iff(var(links(i, t)), var(links(BAUER, t))));
                // Person i ist nachher auf der anderen Seite:
                l.add(iff(var(links(i, t)), var(rechts(i, t + 1))));
                // Bauer ist nachher auf der anderen Seite:
                l.add(iff(var(links(BAUER, t)), var(rechts(BAUER, t + 1))));
                // Alle anderen bleiben wo sie sind:
                for (Person others : Person.values()) {
                    if (i != others && BAUER != others) {
                        l.add(iff(var(links(others, t)), var(links(others, t + 1))));
                    }
                }
                moeglichkeiten.add(and(l));
            }

            conditions.add(or(moeglichkeiten));
        }

        return and(conditions);
    }

    public static void main(String[] args) throws TimeoutException {

        int maxZeit = 7;

        Map<Object, Boolean> eta = satisfiable(bauerHundKatzeMaus(maxZeit));
        if (eta == null) {
            System.out.println("Nicht erfuellbar.");
        } else {
            // erfuellbar
            for (int t = 0; t <= maxZeit; t++) {
                System.out.print("Zeit " + t + ": ");
                for (Person i : Person.values()) {
                    if (eta.get(links(i, t))) {
                        System.out.print("" + i + " ");
                    }
                }
                System.out.print(" |~~~~~Fluss~~~~~| ");
                for (Person i : Person.values()) {
                    if (eta.get(rechts(i, t))) {
                        System.out.print("" + i + " ");
                    }
                }
                System.out.println();
            }
        }
    }
}
