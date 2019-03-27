package meplot.parser;

import meplot.expressions.Expression;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.TokenList;

public class Divided{
	private final DividedNodeList list = new DividedNodeList();
	private final ITokenList restList;

	public Divided(){
		restList = new TokenList();
	}

	public final DividedNodeIterator getIterator(){
		return new DividedNodeIterator(list, 0);
	}

	public final ITokenList rest(){
		return restList;
	}

	public final void add(final char var, final Expression expression){
		list.add(expression, var);
	}

	public final String toString(){
		return '[' + list.toString() + ']' + restList;
	}
}
