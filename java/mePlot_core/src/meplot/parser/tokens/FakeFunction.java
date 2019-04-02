package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.exceptions.FakeException;
import meplot.expressions.functions.Function;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionFunctorVisitor;

public final class FakeFunction extends Function{
	private final int num;

	public FakeFunction(final int needs){
		super(validate(needs));
		num = needs;
	}

	private static Expression[] validate(final int needs){
		if(needs < 0)
			throw new IllegalArgumentException("Negative number of needed arguments");
		return new Expression[needs];
	}

	protected double dvalue(final INumber[] arg){
		throw new FakeException("Fakefunction dvalue");
	}

	public IFunction fill(final Expression[] args){
		throw new FakeException("Fakefunction fill");
	}

	public String getName(){
		return "FAKE" + num;
	}

	protected INumber value(final INumber[] arg){
		throw new FakeException("Fakefunction value");
	}

	public int needs(){
		return num;
	}

    protected double fdvalue(final double[] arg, final char letter, final double value){
		throw new FakeException("Fakefunction fdvalue");
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
