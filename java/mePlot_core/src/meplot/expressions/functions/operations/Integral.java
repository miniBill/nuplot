package meplot.expressions.functions.operations;

import meplot.expressions.Expression;
import meplot.expressions.IValue;
import meplot.expressions.Letter;
import meplot.expressions.functions.Function;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Sum;
import meplot.expressions.visitors.IExpressionFunctorVisitor;

public final class Integral extends Function{
	public Integral(final Expression[] args){
		super(args);
	}

	public Integral(final Expression function, final Expression differential,
			final Expression lower, final Expression upper){
		super(new Expression[]{function, differential, lower, upper}, new boolean[]{true,
				true, false, false});
	}

	private static final int SEGMENTS = 10;

	private INumber nIntegrate(final char var, final double lower, final double upper){
		final IValue function = getArguments()[0];
		final double delta = (upper - lower) / SEGMENTS;
		final Dou codou = new Dou(delta / 6);
		INumber toret = Int.ZERO;
		INumber last = function.value(var, lower);
		double mid = lower + delta / 2.0;
		double right = lower + delta;
		for(int c = 0; c < SEGMENTS; c++){
			final INumber fInMid = Int.FOUR.multiply(function.value(var, mid));
			final INumber fInRight = function.value(var, right);
			final INumber toAdd = codou.multiply(last.add(fInMid).add(fInRight));

			toret = toret.add(toAdd);
			last = fInRight;

			mid += delta;
			right += delta;
		}
		return toret;
	}

	private double dIntegrate(final char var, final double lower, final double upper){
		final IValue function = getArguments()[0];
		final double delta = (upper - lower) / SEGMENTS;
		final double codou = delta / 6.0;
		double toret = 0;
		double last = function.dvalue(var, lower);
		double mid = lower + delta / 2.0;
		double right = lower + delta;
		for(int c = 0; c < SEGMENTS; c++){
			final double fInMid = 4 * function.dvalue(var, mid);
			final double fInRight = function.dvalue(var, right);
			final double toAdd = codou * (last + fInMid + fInRight);

			toret += toAdd;
			last = fInRight;

			mid += delta;
			right += delta;
		}
		return toret;
	}

	protected Expression innerSimplify(final Expression[] vals){
		if(vals[1] instanceof Letter)
			return integrate(vals[0], (Letter)vals[1], vals[2], vals[3]);
		return new Integral(vals);
	}

	private static Expression integrate(final Expression function,
			final Letter differential, final Expression lower, final Expression upper){
		if(!function.hasLetter(differential.getLetter()))
			return function.multiply(upper.add(lower.opposite()));
		if(function instanceof Sum)
			return sumIntegrate((Sum)function, differential, lower, upper);
		return new Integral(function, differential, lower, upper);
	}

	private static Expression sumIntegrate(final Sum function, final Letter differential,
			final Expression lower, final Expression upper){
		final IExpressionIterator iterator = function.getAddends();
		final IExpressionList exprList = new ExpressionList();
		while(iterator.hasNext())
			exprList.add(new Integral(iterator.next(), differential, lower, upper));
		return new Sum(exprList);
	}

	public IFunction fill(final Expression[] args){
		return new Integral(args);
	}

	public String getCategory(){
		return FunctionCategory.OPERATIONS;
	}

	public String getName(){
		return "ii";
	}

	public int needs(){
		return 4;
	}

	private double fdIntegrate(final char var, final double upper, final double lower){
		final IValue function = getArguments()[0];
		final double delta = (upper - lower) / SEGMENTS;
		final double codou = delta / 6.0;
		double toret = 0;
		double last = function.fdvalue(var, lower);
		double mid = lower + delta / 2.0;
		double right = lower + delta;
		for(int c = 0; c < SEGMENTS; c++){
			final double fInMid = 4 * function.fdvalue(var, mid);
			final double fInRight = function.fdvalue(var, right);
			final double toAdd = codou * (last + fInMid + fInRight);

			toret += toAdd;
			last = fInRight;

			mid += delta;
			right += delta;
		}
		return toret;
	}

	protected double dvalue(final INumber[] arg){
		final double lowerVal = arg[2].toDouble();
		final double upperVal = arg[3].toDouble();
		final char var = getArguments()[1].toString().charAt(0);
		if(lowerVal < upperVal)
			return dIntegrate(var, lowerVal, upperVal);
		return -dIntegrate(var, upperVal, lowerVal);
	}

	protected INumber value(final INumber[] arg){
		final double lowerVal = arg[2].toDouble();
		final double upperVal = arg[3].toDouble();
		final char var = getArguments()[1].toString().charAt(0);
		if(lowerVal < upperVal)
			return nIntegrate(var, lowerVal, upperVal);
		return nIntegrate(var, upperVal, lowerVal).inopposite();
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		final char var = getArguments()[1].toString().charAt(0);
		final double lowerVal = arg[2];
		final double upperVal = arg[3];
		if(lowerVal < upperVal)
			return fdIntegrate(var, lowerVal, upperVal);
		return -fdIntegrate(var, upperVal, lowerVal);
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer){
		final Expression[] arguments = getArguments();
		buffer.append("∫_");
		arguments[2].toWrappedHtml(buffer);
		buffer.append('^');
		arguments[3].toWrappedHtml(buffer);
		arguments[0].toWrappedHtml(buffer);
		buffer.append('d');
		buffer.append(arguments[1].toString().charAt(0));
	}
}
