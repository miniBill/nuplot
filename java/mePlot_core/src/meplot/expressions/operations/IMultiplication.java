package meplot.expressions.operations;

import meplot.expressions.Expression;
import meplot.expressions.numbers.INumber;
import platform.lists.IIterable;

public interface IMultiplication extends Iterable<Expression> {
	INumber coefficent();
}
