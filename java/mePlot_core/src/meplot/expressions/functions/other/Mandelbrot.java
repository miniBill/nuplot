package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.functions.exp.Ln;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Mandelbrot extends NonsymbolicFunction{
	public Mandelbrot(final Expression[] expr){
		super(expr);
	}

	public IFunction fill(final Expression[] args){
		return new Mandelbrot(args);
	}

	public String getName(){
		return "mbrot";
	}

	private static final int MAX_ITERATION = 100;

	protected double dvalue(final INumber[] arg){
		final double pointx = arg[0].toDouble();
		final double pointy = arg[1].toDouble();
		return fdvalue(pointx, pointy);
	}

	private static double fdvalue(final double arg0, final double arg1){
		final double firstx = arg0 / 4.0 - 0.5;
		final double firsty = arg1 / 4.0;

		// Common subespressions
		final double xMinus = firstx - 0.25;
		final double xPlus = firstx + 1;

		final double check = Math.sqrt(xMinus * xMinus + firsty * firsty);
		if(firstx < check - 2 * check * check + 0.25
				|| xPlus * xPlus + firsty * firsty < 0.0625)
			return 0;

		double currx = 0;
		double curry = 0;

		int iteration = 0;

		while(currx * currx + curry * curry <= 2 * 2 && iteration < MAX_ITERATION){
			final double xtemp = currx * currx - curry * curry + firstx;
			curry = 2 * currx * curry + firsty;

			currx = xtemp;

			iteration++;
		}

		if(iteration == MAX_ITERATION)
			return 0;
		final double dist = Math.sqrt(currx * currx + curry * curry);
		double count = iteration - ExpMath.ln(ExpMath.ln(dist)) / Ln.LN2;
		while(count > 10)
			count -= 10;
		return count;
	}

	protected INumber value(final INumber[] arg){
		return new Dou(dvalue(arg));
	}

	public int needs(){
		return 2;
	}

	public String getCategory(){
		return FunctionCategory.OTHER;
	}

	protected double fdvalue(final double[] arg){
		return fdvalue(arg[0], arg[1]);
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
