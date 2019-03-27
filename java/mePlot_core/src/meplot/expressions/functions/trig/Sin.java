package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Sin extends NonsymbolicMonicFunction{
	public Sin(final Expression expression){
		super(expression);
	}

	public IFunction fill(final Expression expr){
		return new Sin(expr);
	}

	public INumber value(final INumber arg){
		return TrigMath.sin(arg);
	}

	public double dvalue(final INumber arg){
		return TrigMath.dsin(arg);
	}

	public String getName(){
		return "sin";
	}

	public Expression innerSimplify(final Expression val){
		if(val.isZero())
			return Int.ZERO;
		if(val instanceof Multiplication){
			final Multiplication mval = (Multiplication)val;
			if(mval.getFactors().contains(Int.MINUSONE))
				return fill(val.opposite()).opposite();
		}
		if(val instanceof Letter && ((Letter)val).getLetter() == 'p')
			return Int.ZERO;
		return fill(val);
	}

	public String getCategory(){
		return FunctionCategory.TRIGONOMETRY;
	}

	protected double fdvalue(final double arg){
		return Math.sin(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
