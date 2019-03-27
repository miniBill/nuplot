package meplot.expressions.functions.exp;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Ln extends NonsymbolicMonicFunction{
	public static final double LN2 = 0.69314718056;

	public Ln(final Expression value){
		super(value);
	}

	public boolean isZero(){
		return getArgument().isOne();
	}

	public boolean isOne(){
		final Expression inner = getArgument();
		return inner instanceof Letter && ((Letter)inner).getLetter() == 'e';
	}

	public Expression innerSimplify(final Expression inner){
		if(inner.isOne())
			return Int.ZERO;
		if(inner instanceof Letter && ((Letter)inner).getLetter() == 'e')
			return Int.ONE;
		if(inner instanceof Power){
			final Power pows = (Power)inner;
			if(pows.getBase() instanceof Letter
					&& ((Letter)pows.getBase()).getLetter() == 'e')
				return pows.getExponent();
		}
		if(inner instanceof Dou)
			return ExpMath.ln((Dou)inner);
		return new Ln(inner);
	}

	public INumber value(final INumber val){
		return ExpMath.ln(val);
	}

	public double dvalue(final INumber val){
		return ExpMath.dln(val);
	}

	public IFunction fill(final Expression expression){
		return new Ln(expression);
	}

	public String getName(){
		return "ln";
	}

	public String getCategory(){
		return FunctionCategory.POWER;
	}

	protected double fdvalue(final double arg){
		return ExpMath.ln(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
