package platform.lists;

public final class ListIterator<T> extends Iterator<T> {
    final List<T> inner;

    public ListIterator(List<T> inner, int index) {
        super(index);
        this.inner = inner;
    }

    public ListIterator(List<T> inner) {
        this.inner = inner;
    }

    @Override
    public T next() {
        return inner.elementAt(index++);
    }

    @Override
    protected IIterable<T> getInner() {
        return inner;
    }

    @Override
    public IIterator<T> subIterator() {
        return new ListIterator<T>(inner, index);
    }

    @Override
    public T getCurrent() {
        return inner.elementAt(index);
    }

    @Override
    public T getLast() {
        return inner.elementAt(inner.length() - 1);
    }
}