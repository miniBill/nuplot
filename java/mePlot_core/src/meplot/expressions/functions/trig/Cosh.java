package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.exp.Exp;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import platform.lists.IIterable;

public final class Cosh extends NonsymbolicMonicFunction {
	public Cosh(final Expression expression) {
		super(expression);
	}

	public IFunction fill(final Expression expr) {
		return new Cosh(expr);
	}

	public INumber value(final INumber arg) {
		return TrigMath.cosh(arg);
	}

	public double dvalue(final INumber arg) {
		return TrigMath.dcosh(arg);
	}

	public String getName() {
		return "cosh";
	}

	public Expression innerSimplify(final Expression val) {
		if (val.isZero())
			return Int.ONE;
		if (val instanceof Multiplication) {
			final Multiplication mval = (Multiplication) val;
			if (IIterable.contains(mval, Letter.I))
				return new Cos(val.divide(Letter.I));
		}
		return new Exp(val).add(new Exp(val.opposite())).divide(Int.TWO);
	}

    protected double fdvalue(final double arg) {
		return TrigMath.cosh(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor) {
		return visitor.visit(this);
	}
}
