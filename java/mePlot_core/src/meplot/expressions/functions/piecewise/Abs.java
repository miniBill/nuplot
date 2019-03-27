package meplot.expressions.functions.piecewise;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.complex.AbstractComplexFunction;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Abs extends AbstractComplexFunction{
	public Abs(final Expression value){
		super(value);
	}

	public boolean isOne(){
		return getArgument().isOne();
	}

	public Expression innerSimplify(final Expression valp){
		if(valp.isZero())
			return Int.ZERO;
		if(valp instanceof Sqrt)
			return valp;
		if(valp instanceof IPower){
			final IPower pvalp = (IPower)valp;
			final Expression exp = pvalp.getExponent();
			if(exp instanceof IInt && ((IInt)exp).getValue() % 2 == 0)
				return valp;
		}
		if(valp instanceof Abs)
			return valp;
		if(valp instanceof IReal)
			return ((IReal)valp).abs();
		if(valp instanceof IComplex){
			final IComplex cvalp = (IComplex)valp;
			return new Sqrt(cvalp.real().rsquare().add(cvalp.immaginary().rsquare()));
		}
		return new Abs(valp);
	}

	public IFunction fill(final Expression expression){
		return new Abs(expression);
	}

	public String getName(){
		return "abs";
	}

	public String getCategory(){
		return FunctionCategory.PIECEWISE;
	}

	protected double fdvalue(final double arg){
		return Math.abs(arg);
	}

	protected double dvalue(final INumber arg){
		return FunctionsMath.dabs(arg);
	}

	protected INumber value(final INumber arg){
		return FunctionsMath.abs(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append('|');
		getArgument().toWrappedHtml(buffer);
		buffer.append('|');
	}
}
