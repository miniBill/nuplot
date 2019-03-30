package platform.lists;

public interface IIterable<T> extends Iterable<T> {
    default int length() {
        int result = 0;
        IIterator<T> iterator = getIterator();
        while (iterator.hasNext()) {
            result++;
            iterator.next();
        }
        return result;
    }

    default boolean isEmpty() {
        return !getIterator().hasNext();
    }

    default boolean isSingle() {
        IIterator<T> iterator = getIterator();
        if (!iterator.hasNext())
            return false;
        iterator.next();
        return !iterator.hasNext();
    }

    default T getFirst() {
        return getIterator().next();
    }

    IIterator<T> getIterator();

    default boolean checkContains(IIterable<T> other) {
        final IIterator<T> it = getIterator();
        while (it.hasNext())
            if (!contains(it.next()))
                return false;
        return true;

    }

    default boolean contains(T arg) {
        return contains(arg, 0);
    }

    boolean contains(T arg, int start);
}
