package is.datastructures;

public record Cons<E>(E head, List<E> tail) implements List<E> {

    @Override
    public <R> R accept(ListVisitor<E, R> visitor) {
        return visitor.visit(this);
    }

    @Override
    public String toString() {
        return "Cons(" + head + ", " + tail + ")";
    }
}
