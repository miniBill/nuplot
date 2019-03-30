package platform.database;

import platform.lists.IIterator;
import platform.lists.List;

public class SelectRowIterator implements IIterator<Row> {
	private final IIterator<Row> iterator;
	private final String column;

	public SelectRowIterator(final String column, final IIterator<Row> iterator) {
		this.iterator = iterator;
		this.column = column;
	}

	public Row next() {
		final Row curr = iterator.next();
		return new StringsRow(curr.get(column), column);
	}

	public boolean hasNext() {
		return iterator.hasNext();
	}

	@Override
	public int length() {
		return iterator.length();
	}

	@Override
	public boolean isEmpty() {
		return iterator.isEmpty();
	}

	@Override
	public boolean isSingle() {
		throw new RuntimeException("NIE");
	}

	@Override
	public boolean isSecond() {
		throw new RuntimeException("NIE");
	}

	@Override
	public IIterator<Row> subIterator() {
		throw new RuntimeException("NIE");
	}

	@Override
	public boolean contains(Row value) {
		throw new RuntimeException("NIE");
	}

	@Override
	public Row getLast() {
		throw new RuntimeException("NIE");
	}

	@Override
	public Row getCurrent() {
		throw new RuntimeException("NIE");
	}

	@Override
	public List<Row> until(String string) {
		throw new RuntimeException("NIE");
	}
}
