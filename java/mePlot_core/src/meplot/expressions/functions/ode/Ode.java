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

}
