package is.datastructures;

public interface ListVisitor<E, R> {
    R visit(Nil<E> nil);
    R visit(Cons<E> cons);
}
