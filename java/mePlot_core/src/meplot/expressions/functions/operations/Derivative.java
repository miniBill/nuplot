package meplot.expressions.functions.operations;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.ExpandFunction;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.derivative.DerivativeHelper;

public final class Derivative extends ExpandFunction{
	// ESCA-JAVA0244:
	public Derivative(final Expression[] values){
		super(values, new boolean[]{true, true});
	}

	// ESCA-JAVA0244:
	public Derivative(final Expression expr, final char var){
		// x is most common, use less memory if possible
		super(new Expression[]{expr, var == 'x' ? Letter.X : new Letter(var)});
	}

	protected double dvalue(final INumber[] args, final char letter, final double value){
		return expand().dvalue(letter, value);
	}

	protected INumber value(final INumber[] args, final IValueList letters){
		return expand().value(letters);
	}

	public IFunction fill(final Expression[] args){
		return new Derivative(args);
	}

	public String getCategory(){
		return FunctionCategory.OPERATIONS;
	}

	public String getName(){
		return "dd";
	}

	public int needs(){
		return 2;
	}

	protected Expression innerStepSimplify(final Expression[] args){
		return DerivativeHelper.stepDerivativeOrDefault(args[0], args[1].toString()
				.charAt(0));
	}

	protected Expression expand(final Expression[] args){
		return DerivativeHelper
				.derivativeOrDefault(args[0], args[1].toString().charAt(0));
	}

	protected double dvalue(final INumber[] arg){
		return expand().dvalue();
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		return expand().fdvalue(letter, value);
	}

	protected INumber value(final INumber[] topass){
		return Int.ZERO;
	}
}
