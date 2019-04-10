package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import platform.lists.IIterable;

public final class Cos extends NonsymbolicMonicFunction {
	public Cos(final Expression expression) {
		super(expression);
	}

	public IFunction fill(final Expression expression) {
		return new Cos(expression);
	}

	public INumber value(final INumber arg) {
		return TrigMath.cos(arg);
	}

	public double dvalue(final INumber arg) {
		return TrigMath.dcos(arg);
	}

	public String getName() {
		return "cos";
	}

	public Expression innerSimplify(final Expression val) {
		if (val.isZero())
			return Int.ONE;
		if (val instanceof Multiplication) {
			final Multiplication mval = (Multiplication) val;
			if (IIterable.contains(mval, Int.MINUSONE))
				return fill(val.opposite());
		}
		if (val instanceof Letter && ((Letter) val).getLetter() == 'p')
			return Int.MINUSONE;
		return fill(val);
	}

    protected double fdvalue(final double arg) {
		return Math.cos(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor) {
		return visitor.visit(this);
	}
}
