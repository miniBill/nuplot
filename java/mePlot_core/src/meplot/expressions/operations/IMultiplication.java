package meplot.expressions.operations;

import meplot.expressions.Expression;
import meplot.expressions.numbers.INumber;

public interface IMultiplication extends Iterable<Expression> {
	INumber coefficent();
}
