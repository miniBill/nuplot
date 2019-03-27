package meplot.expressions.geometry;

import meplot.expressions.Expression;
import meplot.expressions.visitors.IExpressionTensorVisitor;

public interface ITensor extends Expression{
	ITensor add(ITensor arg);

	ITensor multiply(ITensor arg);

	ITensor divide(ITensor arg);

	ITensor itinverse();

	Expression accept(IExpressionTensorVisitor visitor);
}
