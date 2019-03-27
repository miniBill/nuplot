package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.functions.ExpandFunction;
import meplot.expressions.functions.MonicFunction;
import meplot.expressions.functions.algebra.Span;
import meplot.expressions.functions.matrix.MatrixFunction;
import meplot.expressions.functions.ode.GenericOde;
import meplot.expressions.functions.operations.Integral;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.parser.tokens.FakeFunction;

public interface IExpressionFunctorVisitor{
	Expression visit(ExpandFunction expandFunction);

	Expression visit(FakeFunction fakeFunction);

	Expression visit(GenericOde genericOde);

	Expression visit(Integral integral);

	Expression visit(MatrixFunction matrixFunction);

	Expression visit(MonicFunction monicFunction);

	Expression visit(NonsymbolicFunction nonsymbolicFunction);

	Expression visit(Span span);
}
