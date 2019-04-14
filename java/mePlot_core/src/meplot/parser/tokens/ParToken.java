package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;

public final class ParToken extends Token{
	private final boolean isOpen;

	public ParToken(final boolean open){
		isOpen = open;
	}

	public String toString(){
		return isOpen() ? "(" : ")";
	}

	public Expression toExpression() throws ParserException{
		throw new ParserException("ParToken toExpression");
	}

	public boolean isOpen(){
		return isOpen;
	}

	public void toString(final StringBuilder buffer){
		if(isOpen())
			buffer.append('(');
		else
			buffer.append(')');
	}
}
