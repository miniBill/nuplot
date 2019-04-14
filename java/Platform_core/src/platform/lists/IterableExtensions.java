package platform.lists;

import platform.NotImplementedException;

import java.lang.reflect.Array;
import java.util.Iterator;

public final class IterableExtensions {
    private IterableExtensions(){}

    @Deprecated
    public static <T> T[] toArray(Class<T> c, Iterable<T> input) {
        T[] result = (T[]) Array.newInstance(c, length(input));
        int i = 0;
        for(T curr : input)
            result[i++] = curr;
        return result;
    }

    public static <T> int length(Iterable<T> input) {
        int result = 0;
        for (T ignored : input)
            result++;
        return result;
    }

    public static <T> boolean isEmpty(Iterable<T> iterable) {
        return !iterable.iterator().hasNext();
    }

    public static <T> boolean isSingle(Iterable<T> iterable) {
        java.util.Iterator<T> iterator = iterable.iterator();
        if (!iterator.hasNext())
            return false;
        iterator.next();
        return !iterator.hasNext();
    }

    public static <T> T getFirst(Iterable<T> iterable) {
        return iterable.iterator().next();
    }

    public static <T> T getLast(Iterable<T> iterable) {
        T result = null;
        for (T curr : iterable)
            result = curr;
        return result;
    }

    public static <T> boolean checkContains(Iterable<T> iterable, Iterable<T> other) {
        for (T curr : other)
            if (!contains(iterable, curr))
                return false;
        return true;
    }

    public static <T> boolean contains(Iterable<T> iterable, T arg) {
        for (T curr : iterable)
            if (curr.equals(arg))
                return true;
        return false;
    }

    public static <T> boolean contains(Iterable<T> iterable, T arg, int start) {
        int i = 0;
        for (T curr : iterable) {
            if (i >= start && curr.equals(arg))
                return true;
            i++;
        }
        return false;
    }

    @Deprecated
    public static <T> Iterable<T> clone(Iterator<T> iterator) {
        if(iterator instanceof ListIterator)
            return ((ListIterator) iterator).clone();
        if(iterator instanceof Myterator)
            return ((Myterator) iterator).clone();
        throw new NotImplementedException();
    }

    public static <T> List<T> until(Iterator<T> iterator, String string) {
        final List<T> toret = new List<>();
        for (T curr : wrap(iterator))
            if (curr.toString().equals(string))
                break;
            else
                toret.add(curr);
        return toret;
    }

    public static <T> Iterable<T> wrap(Iterator<T> iterator) {
        return () -> iterator;
    }

    public static <T> IList<T> toList(T[] input) {
        List<T> result = new List<>();
        for (T t : input)
            result.add(t);
        return result;
    }

    public static <T> void addRange(IList<T> list, Iterable<T> toAdd) {
        for (T t : toAdd)
            list.add(t);
    }

    @Deprecated
    public static <T> void addRange(IList<T> list, Iterator<T> toAdd) {
        addRange(list,wrap(toAdd));
    }
}
