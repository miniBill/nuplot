package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Gcd extends NonsymbolicFunction{
	public Gcd(final Expression[] values){
		super(values);
	}

	public int needs(){
		return 2;
	}

	public String getName(){
		return "gcd";
	}

	public String getCategory(){
		return FunctionCategory.OTHER;
	}

	protected double fdvalue(final double[] arg){
		return FunctionsMath.gcd(arg[0], arg[1]);
	}

	protected double dvalue(final INumber[] arg){
		return FunctionsMath.gcd(arg[0].toDouble(), arg[1].toDouble());
	}

	protected IFunction fill(final Expression[] expression){
		return new Gcd(expression);
	}

	protected INumber value(final INumber[] arg){
		return FunctionsMath.gcd(arg[0], arg[1]);
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
