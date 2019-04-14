package meplot.parser.tokens;

import java.util.Iterator;

public class TokenList extends AbstractTokenList {
	/**
	 * Creates an empty list.
	 */
	public TokenList() {
		// Creates an empty list.
	}

	public TokenList(Iterable<IToken> other) {
		addRange(other);
	}

	public TokenList(final IToken next) {
		add(next);
	}

	public final String toString() {
		final StringBuilder toret = new StringBuilder("{");
		cString(toret);
		toret.append('}');
		return toret.toString();
	}

	public final void addRange(final Iterator<IToken> iterator) {
		while (iterator.hasNext())
			add(iterator.next());
	}

	public final IToken pop() {
		final IToken toret = getLast();
		super.removeAt(length() - 1);
		return toret;
	}

	public final IToken[] toArray() {
		final IToken[] toret = new IToken[length()];
		for (int i = 0; i < toret.length; i++)
			toret[i] = elementAt(i);
		return toret;
	}
}
