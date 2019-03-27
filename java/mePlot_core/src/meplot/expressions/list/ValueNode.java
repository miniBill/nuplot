package meplot.expressions.list;

import meplot.expressions.Expression;

public final class ValueNode implements IValueNode{
	private final char letter;
	private Expression value;

	public ValueNode(final char letter, final Expression expr){
		this.letter = letter;
		value = expr;
	}

	public char getLetter(){
		return letter;
	}

	public void setValue(final Expression val){
		value = val;
	}

	public Expression getValue(){
		return value;
	}
}
