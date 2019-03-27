package meplot.expressions.functions.ode;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.Int;

public final class Ode extends GenericOde{
	public Ode(final Expression[] values){
		super(patch(values));
	}

	private static Expression[] patch(final Expression[] values){
		if(values == null || values.length < 3)
			return new Expression[0];
		return new Expression[]{values[0], values[1], values[2], Int.ONE};
	}

	public IFunction fill(final Expression[] args){
		return new Ode(args);
	}

	public String getName(){
		return "ode";
	}

	public int needs(){
		return 3;
	}

	public String getDescription(){
		return "Can only be used at top level, "
				+ "to enable the drawing of a solution to the Cauchy problem: "
				+ "<ol><li>dy/dt = f(x,t)</li><li>y(x0)=t0</li></ol>.";
	}
}
