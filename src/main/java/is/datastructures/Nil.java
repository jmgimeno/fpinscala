package is.datastructures;

public record Nil<E>() implements List<E> {
    @Override
    public <R> R accept(ListVisitor<E, R> visitor) {
        return visitor.visit(this);
    }

    @Override
    public String toString() {
        return "Nil";
    }
}
