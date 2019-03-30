package meplot.expressions.functions.complex;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.IReal;
import meplot.expressions.visitors.IExpressionComplexFunctionVisitor;
import meplot.help.IHelpFunction;

public final class Arg extends ComplexFunction implements IHelpFunction{
	public Arg(final Expression value){
		super(value);
	}

	public IReal value(final IComplex arg){
		return new Dou(dvalue(arg));
	}

	protected double dvalue(final IComplex arg){
		return arg.arg();
	}

	public IFunction fill(final Expression expr){
		return new Arg(expr);
	}

	public String getName(){
		return "arg";
	}

	protected double fdvalue(final double arg){
		return arg > 0 ? 0 : Math.PI;
	}

	public String argumentName(final int index){
		return null;
	}

	public String argumentDescription(final int index){
		return null;
	}

	public String getDescription(){
		return "Returns the argument of the complex number x";
	}

	public Expression accept(final IExpressionComplexFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
