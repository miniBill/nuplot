package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public abstract class MonicFunction extends Function{
	private boolean isFull;
	private boolean checkDone;

	protected MonicFunction(final Expression value){
		super(new Expression[] {value});
	}

	protected final double dvalue(final INumber[] arg){
		return dvalue(arg[0]);
	}

	protected abstract double dvalue(INumber arg);

	public final IFunction fill(final Expression[] args){
		return fill(args[0]);
	}

	public abstract IFunction fill(Expression expr);

	protected final INumber value(final INumber[] arg){
		return value(arg[0]);
	}

	protected final Expression innerSimplify(final Expression[] vals){
		return innerSimplify(vals[0]);
	}

	protected Expression innerSimplify(final Expression arg){
		if(arg instanceof INumber)
			return value((INumber)arg);
		return fill(arg);
	}

	protected abstract INumber value(INumber arg);

	public final Expression getArgument(){
		return getArguments()[0];
	}

	public final int needs(){
		return 1;
	}

	protected final Expression innerStepSimplify(final Expression[] args){
		return innerStepSimplify(args[0]);
	}

	private Expression innerStepSimplify(final Expression val){
		return innerSimplify(val);
	}

	protected final double fdvalue(final double[] arg, final char letter, final double value){
		return fdvalue(arg[0], letter, value);
	}

	protected abstract double fdvalue(double arg, char letter, double value);

	public final Expression partialSubstitute(final IValueList valueList){
		return fill(getArgument().partialSubstitute(valueList));
	}

	public final Expression partialSubstitute(final char letter, final double value){
		return fill(getArgument().partialSubstitute(letter, value));
	}

	public final Expression partialSubstitute(final char letter, final Expression value){
		return fill(getArgument().partialSubstitute(letter, value));
	}

	public final Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}

	public abstract Expression accept(IExpressionMonicFunctionVisitor visitor);

	public boolean isFullDouble(){
		if(!checkDone){
			isFull = getArgument().isFullDouble();
			checkDone = true;
		}
		return isFull;
	}
}
