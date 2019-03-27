package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Division;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Tan extends NonsymbolicMonicFunction{
	public Tan(final Expression expression){
		super(expression);
	}

	public IFunction fill(final Expression expression){
		return new Tan(expression);
	}

	public INumber value(final INumber arg){
		return TrigMath.tan(arg);
	}

	public double dvalue(final INumber arg){
		return TrigMath.dtan(arg);
	}

	public String getName(){
		return "tan";
	}

	public Expression innerSimplify(final Expression val){
		if(val.isZero())
			return Int.ZERO;
		return new Division(new Sin(val), new Cos(val));
	}

	public String getCategory(){
		return FunctionCategory.TRIGONOMETRY;
	}

	protected double fdvalue(final double arg){
		return Math.tan(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
