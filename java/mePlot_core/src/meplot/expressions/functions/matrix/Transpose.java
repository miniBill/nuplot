package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;

public final class Transpose extends MatrixFunction{
	public Transpose(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "tr";
	}

	public MatrixFunction fill(final Expression expr){
		return new Transpose(expr);
	}

	protected Expression value(final Matrix mat){
		return MatrixMath.transpose(mat);
	}
}
