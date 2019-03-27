package meplot.expressions;

import meplot.expressions.visitors.IVisitable;

public interface Expression extends ISimplifiableValue, ISubstitutible,
		IOutputtable, ICalculable, IVisitable{
	boolean containsMatrix();

	Expression expand();

	boolean hasLetter(char letter);

	boolean isIdentical(Expression other);

	boolean isOpposite(Expression second);

	boolean toStringStartsWith(char prefix);
}
