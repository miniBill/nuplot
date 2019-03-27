package meplot.expressions.list;

import meplot.expressions.Expression;

public interface IExpressionList extends IExpressionIterable{
	void add(Expression expand);

	void addRange(IExpressionIterator iterator);

	IExpressionList fold();

	boolean hasLetter(char letter);

	void addRange(IExpressionList addends);

	String toString(char sep);

	Expression getLast();

	Expression[] toArray();

	Expression elementAt(int index);

	Expression getFirst();
}
