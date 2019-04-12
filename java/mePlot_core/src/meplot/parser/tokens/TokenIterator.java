package meplot.parser.tokens;

import platform.lists.IterableExtensions;
import platform.lists.Myterator;

public final class TokenIterator extends Myterator<IToken> {
	private final ITokenList inner;

	public TokenIterator(final ITokenList head, final int index) {
		super(index);
		inner = head;
	}

	@Override
	public boolean hasNext() {
		return index < IterableExtensions.length(inner);
	}

	public IToken next() {
		return inner.elementAt(index++);
	}

	public IToken peek() {
		return inner.elementAt(index);
	}

	public Iterable<IToken> clone() {
		int currIndex = index;
		return () -> new TokenIterator(inner, currIndex);
	}
}
