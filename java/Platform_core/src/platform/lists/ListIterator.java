package platform.lists;

import java.util.Iterator;

public final class ListIterator<T> extends Myterator<T> {
    final List<T> inner;

    public ListIterator(List<T> inner, int index) {
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

    @Override
    protected Iterable<T> getInner() {
        return inner;
    }

    @Override
    public Iterator<T> iterator() {
        return new ListIterator<>(inner, index);
    }

    public ListIterator<T> clone(){
        return new ListIterator<>(inner, index);
    }
}