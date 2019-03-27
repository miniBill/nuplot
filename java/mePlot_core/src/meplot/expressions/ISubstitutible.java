package meplot.expressions;

import meplot.expressions.list.IValueList;

public interface ISubstitutible{
	Expression applyConstants();

	Expression partialSubstitute(char letter, double value);

	Expression partialSubstitute(char letter, Expression value);

	Expression partialSubstitute(IValueList valueList);
}
