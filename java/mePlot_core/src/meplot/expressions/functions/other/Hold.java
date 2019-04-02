package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public class Hold extends NonsymbolicMonicFunction implements IFunction {
	public Hold(final Expression value){
		super(value);
		simplified = true;
	}

	public String getName(){
		return "hld";
	}

    protected double dvalue(final INumber arg){
		return arg.toDouble();
	}

	public IFunction fill(final Expression expr){
		return new Hold(expr);
	}

	protected INumber value(final INumber arg){
		return arg;
	}

	protected double fdvalue(final double arg){
		return arg;
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
