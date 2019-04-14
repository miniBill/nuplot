package meplot.expressions.functions.complex;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.operations.Sum;
import meplot.expressions.visitors.IExpressionComplexFunctionVisitor;
import platform.lists.IterableExtensions;

import java.util.Iterator;

public final class Re extends ComplexFunction {
	public Re(final Expression value) {
		super(value);
	}

	public IReal value(final IComplex arg) {
		return arg.real();
	}

	public IFunction fill(final Expression expr) {
		return new Re(expr);
	}

	public String getName() {
		return "re";
	}

	protected Expression innerSimplify(final Expression arg) {
		if (arg instanceof Sum)
			return expandSum((Sum) arg);
		if (arg instanceof Multiplication) {
			final Iterator<Expression> iterator = ((Multiplication) arg).iterator();
			if (!iterator.hasNext())
				return Int.ZERO;
			final Expression first = iterator.next();
			final Expression rest = new Multiplication(IterableExtensions.wrap(iterator));
			final ICalculable left = new Re(first).multiply(new Re(rest));
			final ICalculable right = new Im(first).multiply(new Im(rest));
			return left.add(right.multiply(Int.MINUSONE));
		}
		if (arg instanceof INumber)
			return ((INumber) arg).real();
		return fill(arg);
	}

	protected double fdvalue(final double arg) {
		return arg;
	}

	protected double dvalue(final IComplex arg) {
		return arg.real().toDouble();
	}

	public Expression accept(final IExpressionComplexFunctionVisitor visitor) {
		return visitor.visit(this);
	}
}
