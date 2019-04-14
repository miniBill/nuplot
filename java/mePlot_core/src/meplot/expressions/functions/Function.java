package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;

public abstract class Function extends Functor implements IFunction{
	public static boolean[] allTrue(final int count){
		final boolean[] sym = new boolean[count];
		for(int c = 0; c < count; c++)
			sym[c] = true;
		return sym;
	}

	private final boolean[] symbolicArgs;
	private final Expression[] args;

	public final Expression[] getArguments(){
		return args;
	}

	protected Function(final Expression[] arguments){
		args = arguments;
		symbolicArgs = new boolean[args.length];
	}

	protected Function(final Expression[] arguments, final boolean[] symbolicArguments){
		args = arguments;
		symbolicArgs = symbolicArguments;
	}

	/**
	 * {@inheritDoc}
	 */
	public final boolean containsMatrix(){
		for (Expression arg : args)
			if (arg.containsMatrix())
				return true;
		return false;
	}

	public final double dvalue(final char letter, final double value){
		final INumber[] topass = new INumber[args.length];
		for(int c = 0; c < args.length; c++)
			if(!symbolicArgs[c])
				topass[c] = args[c].value(letter, value);
		return dvalue(topass, letter, value);
	}

	protected abstract double dvalue(final INumber[] topass);

	// ESCA-JAVA0173:
	protected double dvalue(final INumber[] topass, final char letter, final double value){
		return dvalue(topass);
	}

	public double fdvalue(final char letter, final double value){
		final double[] topass = new double[args.length];
		for(int c = 0; c < args.length; c++)
			if(!symbolicArgs[c])
				topass[c] = args[c].fdvalue(letter, value);
		return fdvalue(topass, letter, value);
	}

	protected abstract double fdvalue(final double[] arg, char letter, double value);

	/**
	 * {@inheritDoc}
	 */
	public final boolean hasLetter(final char letter){
		for (Expression arg : args)
			if (arg.hasLetter(letter))
				return true;
		return false;
	}

	public final Expression innerSimplify(){
		final Expression[] topass = new Expression[args.length];
		for(int c = 0; c < args.length; c++)
			topass[c] = args[c].partialSimplify();
		return innerSimplify(topass);
	}

	// ESCA-JAVA0173:
	protected Expression innerSimplify(final Expression[] vals){
		return this;
	}

	public final Expression innerStepSimplify(){
		final Expression[] topass = new Expression[args.length];
		boolean changed = false;
		for(int c = 0; c < args.length; c++){
			topass[c] = args[c].innerStepSimplify();
			if(!topass[c].equals(args[c]))
				changed = true;
		}
		if(changed)
			return fill(topass);
		return innerStepSimplify(topass);
	}

	protected Expression innerStepSimplify(final Expression[] args){
		return innerSimplify(args);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean isFullDouble(){
		for (Expression arg : args)
			if (!arg.isFullDouble())
				return false;
		return true;
	}

	public Expression partialSubstitute(final IValueList valueList){
		final Expression[] topass = new Expression[args.length];
		for(int c = 0; c < args.length; c++)
			topass[c] = args[c].partialSubstitute(valueList);
		return fill(topass);
	}

	public Expression partialSubstitute(final char letter, final double value){
		final Expression[] topass = new Expression[args.length];
		for(int c = 0; c < args.length; c++)
			topass[c] = args[c].partialSubstitute(letter, value);
		return fill(topass);
	}

	public Expression partialSubstitute(final char letter, final Expression value){
		final Expression[] topass = new Expression[args.length];
		for(int c = 0; c < args.length; c++)
			topass[c] = args[c].partialSubstitute(letter, value);
		return fill(topass);
	}

	/**
	 * {@inheritDoc}
	 */
	public final void toFullString(final StringBuilder buffer){
		buffer.append(getName());
		buffer.append('(');
		for(int c = 0; c < args.length; c++){
			if(args[c] == null)
				buffer.append('?');
			else
				args[c].toFullString(buffer);
			if(c < args.length - 1)
				buffer.append(',');
		}
		buffer.append(')');
	}

	public void toString(final StringBuilder buffer){
		buffer.append(getName());
		buffer.append('(');
		for(int c = 0; c < args.length - 1; c++){
			args[c].toString(buffer);
			buffer.append(',');
		}
		if(args.length > 0)
			args[args.length - 1].toString(buffer);
		buffer.append(')');
	}

	public final IFunctor gfill(final Expression[] expression){
		return fill(expression);
	}

	protected abstract IFunction fill(Expression[] expression);

	protected abstract INumber value(final INumber[] topass);

	// ESCA-JAVA0173:
	protected INumber value(final INumber[] topass, final IValueList letters){
		return value(topass);
	}

	public final INumber value(final IValueList letters){
		final INumber[] topass = new INumber[args.length];
		for(int c = 0; c < args.length; c++)
			if(!symbolicArgs[c])
				topass[c] = args[c].value(letters);
		return value(topass, letters);
	}
}
