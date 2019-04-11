package meplot.expressions.list;

import meplot.expressions.Expression;

import java.util.Iterator;

public interface IExpressionList extends IExpressionIterable {
	void add(Expression expand);

	void addRange(Iterator<Expression> iterator);

	IExpressionList fold();

	boolean hasLetter(char letter);

	void addRange(IExpressionList addends);

	String toString(char sep);

	Expression getLast();

	Expression[] toArray();
}
