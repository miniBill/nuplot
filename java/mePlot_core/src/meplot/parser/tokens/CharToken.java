package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.numbers.Complex;

public final class CharToken extends Token{
	private final char val;

	public CharToken(final char value){
		val = value;
	}

	public String toString(){
		return String.valueOf(val);
	}

	public Expression toExpression(){
		if(val == 'i')
			return Complex.I;
		return new Letter(val);
	}

	public void toString(final StringBuilder buffer){
		buffer.append(val);
	}
}
