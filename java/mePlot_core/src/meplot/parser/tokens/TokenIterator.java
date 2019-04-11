package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IIterable;
import platform.lists.Myterator;

import java.util.Iterator;

public final class TokenIterator extends Myterator<IToken> {
	private final ITokenList inner;

	public TokenIterator(final ITokenList head, final int index) {
		super(index);
		inner = head;
	}

	@Override
	public boolean hasNext() {
		return index < IIterable.length(inner);
	}

	public IToken next() {
		return inner.elementAt(index++);
	}

	public Expression toExpression() throws ParserException {
		return inner.toExpression(index);
	}

	public IToken peek() {
		return inner.elementAt(index);
	}

	protected Iterable<IToken> getInner() {
		return inner;
	}

	@Override
	public Iterator<IToken> iterator() {
		return new TokenIterator(inner, index);
	}

}
