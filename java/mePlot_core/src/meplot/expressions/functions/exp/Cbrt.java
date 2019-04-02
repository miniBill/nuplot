package meplot.expressions.functions.exp;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import meplot.help.IHelpFunction;

public final class Cbrt extends NonsymbolicMonicFunction implements IHelpFunction{
	public Cbrt(final Expression value){
		super(value);
	}

	public boolean isZero(){
		return getArgument().isZero();
	}

	public boolean isOne(){
		return getArgument().isOne();
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof Cbrt && operation == Operation.MULTIPLICATION)
			return true;
		return super.compatible(elementAt, operation);
	}

	public Expression multiply(final Expression other){
		if(other instanceof Cbrt)
			return new Cbrt(getArgument().multiply(((Cbrt)other).getArgument()));
		return super.multiply(other);
	}

	public Expression innerSimplify(final Expression simplified){
		if(simplified.isZero())
			return Int.ZERO;
		if(simplified.isOne())
			return Int.ONE;
		if(simplified instanceof Power)
			return powerSimplify((Power)simplified);
		if(simplified instanceof IReal)
			return realSimplify((IReal)simplified);
		return new Cbrt(simplified);
	}

	private Expression realSimplify(final IReal simple){
		if(simple instanceof Int){
			int intArg = ((Int)simple).getValue();
			if(intArg < 0)
				return Int.MINUSONE.multiply(new Cbrt(new Int(-intArg)));
			int out = 1;
			while(intArg % 8 == 0){
				out *= 2;
				intArg /= 8;
			}
			for(int i = 3; i < intArg / 2; i++)
				if(intArg % (i * i * i) == 0){
					intArg /= i * i * i;
					out *= i;
				}
			if(out == 1){
				simplified = true;
				return this;
			}
			return new Int(out).multiply(new Cbrt(new Int(intArg)));
		}
		if(simple instanceof Fraction){
			final Fraction frac = (Fraction)simple;
			return new Division(new Cbrt(frac.getNumerator()), new Cbrt(frac.getDenominator()));
		}
		return new Cbrt(simple);
	}

	private static Expression powerSimplify(final Power pps){
		final Expression exp = pps.getExponent();
		if(exp instanceof Int){
			final int exponent = ((Int)exp).getValue();
			if(exponent % 3 == 0)
				return new Power(pps.getBase(), new Int(exponent / 3));
		}
		return new Sqrt(pps);
	}

	public INumber value(final INumber arg){
		return ExpMath.cbrt(arg);
	}

	public double dvalue(final INumber arg){
		if(arg.isReal())
			return ExpMath.dcbrt(arg.real());
		return ExpMath.dcbrt(arg);
	}

	public IFunction fill(final Expression expr){
		return new Cbrt(expr);
	}

	public String getName(){
		return "cbrt";
	}

	protected double fdvalue(final double arg){
		return ExpMath.cbrt(arg);
	}

	public String argumentName(final int index){
		return null;
	}

	public String argumentDescription(final int index){
		return null;
	}

	public String getDescription(){
		return "Returns the cubic root of x";
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
