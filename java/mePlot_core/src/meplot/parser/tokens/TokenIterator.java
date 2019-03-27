package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IEquatableIterable;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

public final class TokenIterator extends ToStringIterator{
	private final ITokenList inner;
	private final int start;

	public TokenIterator(final ITokenList head){
		inner = head;
		start = 0;
	}

	public TokenIterator(final ITokenList head, final int index){
		super(index);
		start = index;
		inner = head;
	}

	public IToken next(){
		return inner.elementAt(index++);
	}

	public Expression toExpression() throws ParserException{
		return inner.toExpression(index);
	}

	public boolean isSecond(){
		return index == start + 1;
	}

	public ITokenList until(final String string){
		final ITokenList toret = new TokenList();
		while(hasNext()){
			final IToken token = next();
			if(token.toString().equals(string))
				break;
			toret.add(token);
		}
		return toret;
	}

	public IToken peek(){
		return inner.elementAt(index);
	}

	public IToString tnext(){
		return next();
	}

	protected IEquatableIterable egetInner(){
		return inner;
	}
}
