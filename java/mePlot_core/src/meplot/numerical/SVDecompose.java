package meplot.numerical;

import meplot.expressions.Expression;
import meplot.expressions.functions.matrix.MatrixFunction;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;
import meplot.expressions.numbers.Int;

public final class SVDecompose extends MatrixFunction{
	public SVDecompose(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "svd";
	}

	public MatrixFunction fill(final Expression expr){
		return new SVDecompose(expr);
	}

	protected Expression value(final Matrix mat){
		final Expression decompose = MatrixMath.svdDecompose(mat);
		return decompose == null ? Int.ZERO : decompose;
	}
}
