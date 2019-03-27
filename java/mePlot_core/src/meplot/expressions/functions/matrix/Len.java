package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.Int;

public final class Len extends MatrixFunction{
	public Len(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "len";
	}

	public MatrixFunction fill(final Expression expr){
		return new Len(expr);
	}

	protected Expression value(final Matrix mat){
		return new Int(mat.getRows());
	}
}
