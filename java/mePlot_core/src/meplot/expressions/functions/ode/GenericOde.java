package meplot.expressions.functions.ode;

import meplot.expressions.Expression;
import meplot.expressions.functions.Function;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.help.IHelpFunction;
import meplot.numerical.RungeKutta;

public class GenericOde extends Function implements IHelpFunction{
	public GenericOde(final Expression[] values){
		super(values, new boolean[]{true, false, true, true});
	}

	public IFunction fill(final Expression[] args){
		return new GenericOde(args);
	}

	public final String getCategory(){
		return FunctionCategory.OTHER;
	}

	public String getName(){
		return "gode";
	}

	public int needs(){
		return 4;
	}

	public final RungeKutta getRK4(){
		return new RungeKutta(getArguments()[0], getArguments()[1].dvalue(), getArguments()[2],
				getArguments()[3]);
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

	public final String argumentName(final int index){
		switch(index){
			case 0:
				return "f";
			case 1:
				return "t0";
			case 2:
				return "x0";
			default:
				return "l";
		}
	}

	public final String argumentDescription(final int index){
		switch(index){
			case 0:
				return "An expression, containing x and/or t";
			case 1:
				return "Initial t value";
			case 2:
				return "Initial x value";
			default:
				return "Post-operation";
		}
	}

	public String getDescription(){
		return "Can only be used at top level, "
				+ "to enable the drawing of a solution to the Cauchy problem: "
				+ "<ol><li>dy/dt = f(x,t)</li><li>y(x0)=t0</li></ol>. "
				+ "The l parameter is pre-multiplied to the result before the drawing. "
				+ "This is useful because you can put a matrix in there, for multiple vars, "
				+ "or a lambda expression x=>(f(x))";
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
