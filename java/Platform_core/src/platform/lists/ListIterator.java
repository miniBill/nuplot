package platform.lists;

public final class ListIterator<T> extends Myterator<T> {
    final IList<T> inner;

    public ListIterator(IList<T> inner, int index) {
        super(index);
        this.inner = inner;
    }

    public ListIterator(List<T> inner) {
        this.inner = inner;
    }

    @Override
    public boolean hasNext() {
        return index < inner.length();
    }

    @Override
    public T next() {
        return inner.elementAt(index++);
    }

    public Iterable<T> clone() {
        int currIndex = index;
        return () -> new ListIterator<>(inner, currIndex);
    }
}