package platform.lists;

import com.sun.istack.internal.NotNull;

import java.lang.reflect.Array;

public interface IIterable<T> extends Iterable<T> {
    static <T> T[] toArray(Class<T> c, IIterable<T> input) {
        T[] result = (T[]) Array.newInstance(c, length(input));
        int i = 0;
        for(T curr : input)
            result[i++] = curr;
        return result;
    }

    static <T> int length(Iterable<T> input) {
        int result = 0;
        for (T ignored : input)
            result++;
        return result;
    }

    static <T> boolean isEmpty(Iterable<T> iterable) {
        return !iterable.iterator().hasNext();
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

    default T getLast() {
        T result = null;
        for (T curr : this)
            result = curr;
        return result;
    }

    @NotNull
    default java.util.Iterator<T> iterator() {
        return getIterator();
    }

    @NotNull
    IIterator<T> getIterator();

    default boolean checkContains(IIterable<T> other) {
        for (T curr : this)
            if (!contains(curr))
                return false;
        return true;

    }

    default boolean contains(T arg) {
        return contains(arg, 0);
    }

    default boolean contains(T arg, int start) {
        int i = 0;
        for (T curr : this) {
            if (i >= start && curr.equals(arg))
                return true;
            i++;
        }
        return false;
    }
}
