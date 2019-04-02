package platform.database;

import platform.lists.IIterator;
import platform.lists.List;

public class FilterRowIterator implements IIterator<Row> {
	private final IIterator<Row> inner;
	private Row temp;
	private final String column;
	private final String value;

	public FilterRowIterator(final String column, final String value, final IIterator<Row> iterator) {
		this.column = column;
		this.value = value;
		inner = iterator;
	}

	public Row next() {
		if (!hasNext())
			return null;

		final Row toret = temp;
		temp = null;
		return toret;
	}

	public boolean hasNext() {
		if (temp != null)
			return true;
		do {
			temp = inner.next();
			if (temp == null)
				return false;
			if (temp.get(column).equals(value))
				return true;
		} while (inner.hasNext());
		temp = null;
		return false;
	}

	@Override
	public int length() {
		throw new RuntimeException("NIE");
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
