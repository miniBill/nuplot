package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IDou;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.other.Poly;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public final class FunctionsMath{
	public static Dou abs(final INumber arg){
		return new Dou(dabs(arg));
	}

	public static double dabs(final INumber arg){
		if(arg.isReal())
			return Math.abs(arg.toDouble());
		return arg.toComplex().norm();
	}

	public static double dsign(final INumber arg){
		if(arg.isReal()){
			final IReal real = arg.real();
			if(real.isPositive())
				return 1;
			else
				if(real.isNegative())
					return -1;
				else
					return 0;
		}
		return 0;
	}

	public static int floor(final double arg){
		return (int)Math.floor(arg);
	}

	public static IReal floor(final INumber arg){
		if(!arg.isReal())
			return Int.ZERO;
		return new Int(floor(arg.toDouble()));
	}

	public static Expression gcd(final Expression arg0, final Expression arg1){
		if(arg0 instanceof INumber && arg1 instanceof INumber)
			return gcd((INumber)arg0, (INumber)arg1);
		if(!(arg0 instanceof Poly) && Poly.isPoly(arg0, 'x'))
			return gcd(new Poly(arg0, 'x'), arg1); // TODO : Fix: generalize
		if(!(arg1 instanceof Poly) && Poly.isPoly(arg1, 'x'))
			return gcd(arg0, new Poly(arg1, 'x')); // TODO : Fix: generalize
		if(arg0 instanceof Poly && arg1 instanceof Poly)
			return gcd((Poly)arg0, (Poly)arg1);
		throw new CalcException("Dunno how to calculate gcd of " + arg0.toFullString()
				+ " and " + arg1.toFullString());
	}

	public static INumber gcd(final INumber arg0, final INumber arg1){
		if(arg0 instanceof IInt && arg1 instanceof IInt)
			return gcd((IInt)arg0, (IInt)arg1);
		if(arg0 instanceof IDou && arg1 instanceof IDou)
			return gcd((IDou)arg0, (IDou)arg1);
		throw new CalcException("Dunno how to calculate gcd of " + arg0.toFullString()
				+ " and " + arg1.toFullString());
	}

	private static IInt gcd(final IDou arg0, final IDou arg1){
		return new Int(gcd(arg0.toDouble(), arg1.toDouble()));
	}

	public static int gcd(final int arg0, final int arg1){
		int left = arg0;
		int right = arg1;
		for(int check = 0; check < 200; check++){
			if(left == 0)
				return right;
			if(right == 0)
				return left;
			if(left > right)
				left -= right * (int)Math.floor((double)left / right);
			else{
				final int temp = right - left * (int)Math.floor((double)right / left);
				right = left;
				left = temp;
			}
		}
		return 1;
	}

	private static IInt gcd(final IInt arg0, final IInt arg1){
		if(arg1.isZero())
			return arg0;
		if(arg0.isZero())
			return arg1;
		return new Int(gcd(arg0.getValue(), arg1.getValue()));
	}

	public static Poly gcd(final Poly arg0, final Poly arg1){
		if(arg1.isZero())
			return arg0;
		if(arg0.isZero())
			return arg1;
		if(arg1.getDegree() == 0)
			return arg1;
		if(arg0.getDegree() == 0)
			return arg0;
		if(arg0.equals(arg1))
			return arg0;
		final Poly mod = arg0.mod(arg1);
		final Poly sim = mod.psimplify();
		return gcd(arg1, sim);
	}

	public static Expression lcm(final Expression arg0, final Expression arg1){
		if(arg0 instanceof Int && arg1 instanceof Int)
			return lcm((Int)arg0, (Int)arg1);
		if(arg0.equals(arg1))
			return arg0;
		return arg0.multiply(arg1);
	}

	public static IInt lcm(final IInt arg0, final IInt arg1){
		INumber toret = arg0.multiply(arg1).divide(gcd(arg0, arg1));
		return (IInt)SimplificationHelper.simplify(toret);
	}

	public static INumber sign(final INumber arg){
		return arg.real().isPositive() ? Int.ONE : arg.real().isNegative() ? Int.MINUSONE
				: Int.ZERO;
	}

	private FunctionsMath(){
	}

	public static int gcd(final double arg0, final double arg1){
		return gcd((int)arg0, (int)arg1);
	}
}
