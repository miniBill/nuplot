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

import java.util.Iterator;

public final class Im extends ComplexFunction {
	public Im(final Expression value) {
		super(value);
	}

	public IReal value(final IComplex arg) {
		return arg.immaginary();
	}

	public IFunction fill(final Expression expression) {
		return new Im(expression);
	}

	public String getName() {
		return "im";
	}

	protected Expression innerSimplify(final Expression arg) {
		if (arg instanceof Sum)
			return expandSum((Sum) arg);
		if (arg instanceof Multiplication) {
			final Iterator<Expression> iterator = ((Multiplication) arg).iterator();
			if (!iterator.hasNext())
				return Int.ZERO;
			final Expression first = iterator.next();
			final Expression rest = new Multiplication(iterator);
			final ICalculable left = new Im(first).multiply(new Re(rest));
			final Expression right = new Re(first).multiply(new Im(rest));
			return left.add(right);
		}
		if (arg instanceof INumber)
			return ((INumber) arg).toComplex().immaginary();
		return fill(arg);
	}

	public Expression accept(final IExpressionComplexFunctionVisitor visitor) {
		return visitor.visit(this);
	}

	protected double fdvalue(final double arg) {
		return 0;
	}

	protected double dvalue(final IComplex arg) {
		return arg.immaginary().toDouble();
	}
}
