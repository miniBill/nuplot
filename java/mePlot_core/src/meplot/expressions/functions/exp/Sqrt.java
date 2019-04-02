package meplot.expressions.functions.exp;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.IDivision;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Sqrt extends NonsymbolicMonicFunction{
	public Sqrt(final Expression value){
		super(value);
	}

	public boolean isZero(){
		return getArgument().isZero();
	}

	public boolean isOne(){
		return getArgument().isOne();
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof Sqrt && operation == Operation.MULTIPLICATION)
			return true;
		return super.compatible(elementAt, operation);
	}

	public Expression multiply(final Expression other){
		if(other instanceof Sqrt)
			return new Sqrt(getArgument().multiply(((Sqrt)other).getArgument()));
		return super.multiply(other);
	}

	public Expression innerSimplify(final Expression simplified){
		if(simplified.isZero())
			return simplified;
		if(simplified.isOne())
			return simplified;
		if(simplified instanceof Power)
			return powerSimplify((Power)simplified);
		if(simplified instanceof IDivision){
			final IDivision div = (IDivision)simplified;
			return new Sqrt(div.getNumerator()).divide(new Sqrt(div.getDenominator()));
		}
		if(simplified instanceof IReal)
			return realSimplify((IReal)simplified);
		return new Sqrt(simplified);
	}

	private Expression realSimplify(final IReal simple){
		if(simple instanceof IInt){
			int intArg = ((IInt)simple).getValue();
			if(intArg < 0)
				return Complex.I.multiply(new Sqrt(new Int(-intArg)));
			int out = 1;
			while(intArg % 4 == 0){
				out *= 2;
				intArg /= 4;
			}
			for(int i = 3; i < intArg / 2; i++)
				if(intArg % (i * i) == 0){
					intArg /= i * i;
					out *= i;
				}
			if(out == 1){
				simplified = true;
				return this;
			}
			return new Int(out).multiply(new Sqrt(new Int(intArg)));
		}
		return new Sqrt(simple);
	}

	private static Expression powerSimplify(final Power pps){
		final Expression exp = pps.getExponent();
		if(exp instanceof IInt){
			final int exponent = ((IInt)exp).getValue();
			if(exponent % 2 == 0)
				return new Power(new Abs(pps.getBase()), new Int(exponent / 2));
		}
		return new Sqrt(pps);
	}

	public INumber value(final INumber arg){
		return ExpMath.sqrt(arg);
	}

	public double dvalue(final INumber arg){
		return ExpMath.dsqrt(arg);
	}

	public IFunction fill(final Expression expr){
		return new Sqrt(expr);
	}

	public String getName(){
		return "sqrt";
	}

	public Expression square(){
		final Expression argument = getArgument();
		if(argument instanceof IReal)
			return ((IReal)argument).abs();
		return new Abs(argument);
	}

	protected double fdvalue(final double arg){
		return Math.sqrt(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append('√');
		getArgument().toWrappedHtml(buffer);
	}
}
