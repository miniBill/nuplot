package meplot.expressions.operations;

import meplot.expressions.Expression;
import meplot.expressions.numbers.INumber;
import platform.lists.IIterator;

public interface IMultiplication {
	IIterator<Expression> getFactors();

	INumber coefficent();
}
