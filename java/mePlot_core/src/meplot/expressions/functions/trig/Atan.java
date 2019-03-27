package meplot.expressions.functions.trig;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Atan extends NonsymbolicMonicFunction{
	public Atan(final Expression value){
		super(value);
	}

	public IFunction fill(final Expression expr){
		return new Atan(expr);
	}

	protected INumber value(final INumber arg){
		return TrigMath.atan(arg);
	}

	protected double dvalue(final INumber arg){
		return TrigMath.datan(arg);
	}

	public String getName(){
		return "atan";
	}

	public String getCategory(){
		return FunctionCategory.TRIGONOMETRY;
	}

	protected double fdvalue(final double arg){
		return TrigMath.atan(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
