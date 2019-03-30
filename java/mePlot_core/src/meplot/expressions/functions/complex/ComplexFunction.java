package meplot.expressions.functions.complex;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.operations.Sum;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import platform.lists.IIterator;

public abstract class ComplexFunction extends AbstractComplexFunction {
	protected ComplexFunction(final Expression value) {
		super(value);
	}

	protected final double dvalue(final INumber arg) {
		return dvalue(arg.toComplex());
	}

	protected abstract double dvalue(final IComplex arg);

	protected final INumber value(final INumber arg) {
		return value(arg.toComplex());
	}

	protected abstract IReal value(IComplex arg);

	public final String getCategory() {
		return FunctionCategory.COMPLEX;
	}

	protected final Expression expandSum(final Sum sarg) {
		final IIterator<Expression> iterator = sarg.getAddends();
		final IExpressionList toret = new ExpressionList();
		while (iterator.hasNext())
			toret.add(fill(iterator.next()));
		return new Sum(toret);
	}

	public final Expression accept(final IExpressionMonicFunctionVisitor visitor) {
		return visitor.visit(this);
	}
}
