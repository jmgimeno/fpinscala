package is.datastructures;

public sealed interface List<E> permits Nil, Cons {
    <R> R accept(ListVisitor<E, R> visitor);
    
    @SafeVarargs
    static <E> List<E> apply(E... elems) {
        List<E> list = new Nil<>();
        for (int i = elems.length - 1; i >= 0; i--) {
            list = new Cons<>(elems[i], list);
        }
        return list;
    }
}
