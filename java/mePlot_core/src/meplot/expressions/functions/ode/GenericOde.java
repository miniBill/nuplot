package meplot.expressions.functions.ode;

import meplot.expressions.Expression;
import meplot.expressions.functions.Function;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.numerical.RungeKutta;

public class GenericOde extends Function implements IFunction {
	public GenericOde(final Expression[] values){
		super(values, new boolean[]{true, false, true, true});
	}

	public IFunction fill(final Expression[] args){
		return new GenericOde(args);
	}

    public String getName(){
		return "gode";
	}

	public int needs(){
		return 4;
	}

	protected final double dvalue(final INumber[] arg){
		return 0;
	}

	protected final double fdvalue(final double[] arg, final char letter, final double value){
		return 0;
	}

	protected final INumber value(final INumber[] topass){
		return Int.ZERO;
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
