package meplot.parser;

import meplot.expressions.Expression;

final class DividedNode{
	private final Expression value;
	private final char letter;

	DividedNode(final Expression expr, final char name){
		value = expr;
		letter = name;
	}

	public Expression getValue(){
		return value;
	}

	public char getLetter(){
		return letter;
	}
}
