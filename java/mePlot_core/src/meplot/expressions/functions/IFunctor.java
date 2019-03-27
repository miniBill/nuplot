package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.visitors.IExpressionFunctorVisitor;

public interface IFunctor extends Expression{
	/**
	 * Returns the number of arguments the function needs. The function returns
	 * -1 to say that it can accept an arbitrary number of arguments.
	 *
	 * @return The number of arguments the function needs.
	 */
	int needs();

	/**
	 * Creates a new function object, with the given arguments.
	 *
	 * @param args
	 *            The arguments to the new function object.
	 * @return The new function object.
	 */
	IFunctor gfill(Expression[] args);

	/**
	 * Returns the name of the function.
	 *
	 * @return The name of the function.
	 */
	String getName();

	/**
	 * Returns the category of the function.
	 *
	 * @see FunctionCategory
	 * @return The category of the function.
	 */
	String getCategory();

	Expression[] getArguments();

	Expression accept(IExpressionFunctorVisitor visitor);
}
