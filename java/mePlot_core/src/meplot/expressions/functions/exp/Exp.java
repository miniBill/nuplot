package meplot.expressions.functions.exp;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.OperationsMath;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Exp extends NonsymbolicMonicFunction{
	public Exp(final Expression value){
		super(value);
	}

	public Expression innerSimplify(final Expression val){
		return new Power(Letter.E, val);
	}

	public INumber value(final INumber arg){
		return ExpMath.exp(arg);
	}

	protected double dvalue(final INumber arg){
		return ExpMath.dexp(arg);
	}

	public IFunction fill(final Expression expression){
		return new Exp(expression);
	}

	public String getName(){
		return "exp";
	}

    protected double fdvalue(final double arg){
		return OperationsMath.pow(Math.E, arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
