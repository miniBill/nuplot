package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;

public final class Rank extends MatrixFunction{
	public Rank(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "rk";
	}

	public MatrixFunction fill(final Expression expr){
		return new Rank(expr);
	}

	protected Expression value(final Matrix mat){
		return MatrixMath.rank(mat);
	}
}
