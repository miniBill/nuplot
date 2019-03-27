package meplot.expressions.operations;

import meplot.expressions.list.IExpressionIterator;
import meplot.expressions.numbers.INumber;

public interface IMultiplication{
	IExpressionIterator getFactors();

	INumber coefficent();
}
