package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.INumber;

public interface IExpressionTensorVisitor{
	Expression visit(INumber number);

	Expression visit(Matrix matrix);
}
