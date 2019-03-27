package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionTensorVisitor;

public class DerivativeTensorVisitor implements IExpressionTensorVisitor{
	private final DerivativeVisitor parent;

	public DerivativeTensorVisitor(final DerivativeVisitor parent){
		this.parent = parent;
	}

	public Expression visit(final INumber number){
		return Int.ZERO;
	}

	public Expression visit(final Matrix matrix){
		if(matrix.getCols() == 1){
			final Expression[][] dvals = new Expression[matrix.getRows()][1];
			for(int x = 0; x < matrix.getRows(); x++)
				dvals[x][0] = parent.genvisit(matrix.get(x, 0));
			return new Matrix(dvals);
		}
		return Int.ZERO;
	}
}
