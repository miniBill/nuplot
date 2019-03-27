package meplot.expressions.visitors;

import meplot.expressions.Expression;

public interface IVisitable{
	Expression accept(IExpressionVisitor visitor);
}
