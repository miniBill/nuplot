package platform.lists;

public interface IList<T> extends Iterable<T> {
    void add(T element);

    T elementAt(int index);

    int length();
}
