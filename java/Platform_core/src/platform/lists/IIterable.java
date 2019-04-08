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

    static <T> boolean isSingle(Iterable<T> iterable) {
        java.util.Iterator<T> iterator = iterable.iterator();
        if (!iterator.hasNext())
            return false;
        iterator.next();
        return !iterator.hasNext();
    }

    static <T> T getFirst(Iterable<T> iterable) {
        return iterable.iterator().next();
    }

    static <T> T getLast(IIterable<T> iterable) {
        T result = null;
        for (T curr : iterable)
            result = curr;
        return result;
    }

    @NotNull
    default java.util.Iterator<T> iterator() {
        return getIterator();
    }

    @NotNull
    IIterator<T> getIterator();

    static <T> boolean checkContains(Iterable<T> iterable, Iterable<T> other) {
        for (T curr : other)
            if (!contains(iterable, curr))
                return false;
        return true;
    }

    static <T> boolean contains(Iterable<T> iterable, T arg) {
        return contains(iterable, arg, 0);
    }

    static <T> boolean contains(Iterable<T> iterable, T arg, int start) {
        int i = 0;
        for (T curr : iterable) {
            if (i >= start && curr.equals(arg))
                return true;
            i++;
        }
        return false;
    }
}
