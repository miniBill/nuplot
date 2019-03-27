package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunctor;
import meplot.expressions.functions.piecewise.IPower;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.IDivision;
import meplot.expressions.operations.IMultiplication;
import meplot.expressions.operations.Lambda;
import meplot.expressions.operations.Sum;
import meplot.expressions.other.Proxy;

public interface IExpressionVisitor{
	Expression visit(BooleanOp booleanOp);

	Expression visit(IDivision division);

	Expression visit(IFunctor functor);

	Expression visit(IMultiplication multiplication);

	Expression visit(IPower power);

	Expression visit(ITensor tensor);

	Expression visit(Lambda lambda);

	Expression visit(Letter letter);

	Expression visit(Proxy proxy);

	Expression visit(Sum sum);
}
