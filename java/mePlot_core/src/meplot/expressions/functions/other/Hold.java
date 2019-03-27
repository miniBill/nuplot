package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import meplot.help.IHelpFunction;

public class Hold extends NonsymbolicMonicFunction implements IHelpFunction{
	public Hold(final Expression value){
		super(value);
		simplified = true;
	}

	public String getName(){
		return "hld";
	}

	public String getCategory(){
		return FunctionCategory.OTHER;
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

	public String argumentName(final int index){
		return "expr";
	}

	public String argumentDescription(final int index){
		return "The expression to hold";
	}

	public String getDescription(){
		return "Prevents simplification of the argument";
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
