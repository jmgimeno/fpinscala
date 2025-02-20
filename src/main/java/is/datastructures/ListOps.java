package is.datastructures;

public class ListOps {

    public static int sum(List<Integer> ints) {
        return ints.accept(new ListVisitor<>() {
            @Override
            public Integer visit(Nil<Integer> nil) {
                return 0;
            }

            @Override
            public Integer visit(Cons<Integer> cons) {
                return cons.head() + cons.tail().accept(this);
            }
        });
    }

    public static int sum2(List<Integer> ints) {
        if (ints instanceof Nil) {
            return 0;
        } else if (ints instanceof Cons(var head, var tail)) {
            return head + sum2(tail);
        } else {
            throw new IllegalStateException("cannot happen");
        }
    }

    public static int sum3(List<Integer> ints) {
        return switch(ints) {
            case Nil<?> n -> 0; // In Java 23 one can use _ 
            case Cons(var head, var tail) -> head + sum3(tail);
        };
    }
}
