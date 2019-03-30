package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IIterable;
import platform.lists.IIterator;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

public final class TokenIterator extends ToStringIterator<IToken> {
	private final ITokenList inner;

	public TokenIterator(final ITokenList head) {
		inner = head;
		start = 0;
	}

	public TokenIterator(final ITokenList head, final int index) {
		super(index);
		start = index;
		inner = head;
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

	public IToString tnext() {
		return next();
	}

	protected IIterable<IToken> getInner() {
		return inner;
	}

	@Override
	public IIterator<IToken> subIterator() {
		throw new RuntimeException("NIE");
	}

	@Override
	public IToken getLast() {
		throw new RuntimeException("NIE");
	}

	@Override
	public IToken getCurrent() {
		throw new RuntimeException("NIE");
	}
}
