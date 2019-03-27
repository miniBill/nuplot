package meplot.numerical;

import meplot.expressions.Expression;
import meplot.expressions.functions.matrix.MatrixFunction;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;

public final class QRDecompose extends MatrixFunction{
	public QRDecompose(final Expression expr){
		super(expr);
	}

	public String getName(){
		return "qr";
	}

	public MatrixFunction fill(final Expression expr){
		return new QRDecompose(expr);
	}

	protected Expression value(final Matrix mat){
		return MatrixMath.qrDecompose(mat);
	}
}
