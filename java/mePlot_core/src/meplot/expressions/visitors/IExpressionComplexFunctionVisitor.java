package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.functions.complex.Arg;
import meplot.expressions.functions.complex.Im;
import meplot.expressions.functions.complex.Re;

public interface IExpressionComplexFunctionVisitor{
	Expression visit(Arg arg);

	Expression visit(Im im);

	Expression visit(Re re);
}
