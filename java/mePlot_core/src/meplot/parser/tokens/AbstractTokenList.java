package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IToStringIterator;
import platform.lists.ToStringList;

public abstract class AbstractTokenList extends ToStringList implements ITokenList{
	public final void add(final IToken token){
		super.add(token);
	}

	public final Expression toExpression() throws ParserException{
		return toExpression(0);
	}

	protected final IToStringIterator tgetIterator(final int index){
		return getIterator(index);
	}

	public final TokenIterator getIterator(){
		return new TokenIterator(this);
	}

	protected final TokenIterator getIterator(final int index){
		return new TokenIterator(this, index);
	}

	public final String toCString(){
		final StringBuffer toret = new StringBuffer();
		cString(toret);
		return toret.toString();
	}

	public final String toSString(){
		final StringBuffer toret = new StringBuffer("{");
		final TokenIterator iterator = getIterator();
		while(iterator.hasNext()){
			final IToken curr = iterator.next();
			if(curr instanceof TokenList)
				toret.append(curr.toCString());
			else
				toret.append(curr.toString());
			if(iterator.hasNext())
				toret.append(',');
		}
		toret.append('}');
		return toret.toString();
	}

	protected final void cString(final StringBuffer toret){
		final TokenIterator iterator = getIterator();
		while(iterator.hasNext())
			toret.append(iterator.next());
	}

	public final IToken getLast(){
		return (IToken)ggetLast();
	}

	public final IToken elementAt(final int index){
		return (IToken)gelementAt(index);
	}

	// Needed for broken real implementations.
	public abstract Expression toExpression(final int index) throws ParserException;
}
