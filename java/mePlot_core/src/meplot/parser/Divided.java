package meplot.parser;

import meplot.expressions.Expression;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.TokenList;
import platform.lists.IIterator;

public class Divided {
	private final DividedNodeList list = new DividedNodeList();
	private final ITokenList restList;

	public Divided() {
		restList = new TokenList();
	}

	public final IIterator<DividedNode> getIterator() {
		return list.getIterator();
	}

	public final ITokenList rest() {
		return restList;
	}

	public final void add(final char var, final Expression expression) {
		list.add(expression, var);
	}

	public final String toString() {
		return '[' + list.toString() + ']' + restList;
	}
}
