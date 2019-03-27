package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;

public final class Det extends MatrixFunction{
	public Det(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "det";
	}

	public MatrixFunction fill(final Expression expr){
		return new Det(expr);
	}

	protected Expression value(final Matrix mat){
		return MatrixMath.det(mat);
	}
}
