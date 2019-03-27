package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;

public final class Gradient extends MatrixFunction{
	public Gradient(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "gra";
	}

	public MatrixFunction fill(final Expression expr){
		return new Gradient(expr);
	}

	protected Expression value(final Matrix mat){
		return mat.gradient();
	}
}
