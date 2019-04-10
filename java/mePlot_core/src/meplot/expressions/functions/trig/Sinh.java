package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.exp.Exp;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import platform.lists.IIterable;

public final class Sinh extends NonsymbolicMonicFunction {
	public Sinh(final Expression expression) {
		super(expression);
	}

	public IFunction fill(final Expression expr) {
		return new Sinh(expr);
	}

	public INumber value(final INumber arg) {
		return TrigMath.sinh(arg);
	}

	public double dvalue(final INumber arg) {
		return TrigMath.dsinh(arg);
	}

	public String getName() {
		return "sinh";
	}

	public Expression innerSimplify(final Expression val) {
		if (val.isZero())
			return Int.ZERO;
		if (val instanceof Multiplication) {
			final Multiplication mval = (Multiplication) val;
			if (IIterable.contains(mval, Letter.I))
				return new Sin(val.divide(Letter.I)).multiply(Letter.I);
		}
		final ICalculable left = new Exp(val);
		final Expression right = new Exp(val.opposite()).opposite();
		return left.add(right).divide(Int.TWO);
	}

    protected double fdvalue(final double arg) {
		return TrigMath.sinh(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor) {
		return visitor.visit(this);
	}
}
