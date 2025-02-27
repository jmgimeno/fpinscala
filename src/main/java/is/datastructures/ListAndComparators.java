package is.datastructures;

import java.util.Comparator;
import java.util.List;
import java.util.NoSuchElementException;

public class ListAndComparators {
    
    public static <A> A min(List<A> list, Comparator<? super A> cmp) {
        if (list.isEmpty())
            throw new NoSuchElementException("min of empty list");
        var min = list.getFirst();
        for (var elem: list) {
            if (cmp.compare(elem, min) < 0)
                min = elem;
        }
        return min;
    }

    public static void main(String[] args) {
        var l = List.of(3, 4, 1, 2, 5);
        var min = min(l, Comparator.naturalOrder());
        System.out.printf("The min of %s is %d%n", l, min);
        var max = min(l, Comparator.<Integer>naturalOrder().reversed());
        System.out.printf("The max of %s is %d%n", l, max);
        
    }
}
