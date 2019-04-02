package meplot.expressions.functions.piecewise;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Piecewise extends NonsymbolicFunction{
	public Piecewise(final Expression[] args){
		super(args);
	}

	public Piecewise(final Expression condition, final Expression whatif,
			final Expression whatnot){
		super(new Expression[]{condition, whatif, whatnot});
	}

	public Expression getCondition(){
		return getArguments()[0];
	}

	public Expression getIf(){
		return getArguments()[1];
	}

	public Expression getIfnot(){
		return getArguments()[2];
	}

	public IFunction fill(final Expression[] args){
		return new Piecewise(args);
	}

	public String getName(){
		return "pw";
	}

	protected INumber value(final INumber[] arg){
		return arg[0].real().isPositive() ? arg[1] : arg[2];
	}

	public int needs(){
		return 3;
	}

    protected Expression innerSimplify(final Expression[] vals){
		final Expression simif = vals[1];
		final Expression simifnot = vals[2];
		if(simif.equals(simifnot))
			return simif;
		final Expression simcondition = vals[0];
		if(simcondition instanceof BooleanOp){
			final BooleanOp bcondition = (BooleanOp)simcondition;
			if(bcondition.isDecided())
				return bcondition.isFinallyTrue() ? simif : simifnot;
		}
		if(simcondition instanceof INumber){
			if(((INumber)simcondition).real().isPositive())
				return simif;
			return simifnot;
		}
		return new Piecewise(new Expression[]{simcondition, simif, simifnot});
	}

	protected double fdvalue(final double[] arg){
		return arg[0] > 0 ? arg[1] : arg[2];
	}

	protected double dvalue(final INumber[] arg){
		return arg[0].real().isPositive() ? arg[1].toDouble() : arg[2].toDouble();
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
