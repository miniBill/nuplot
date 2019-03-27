package meplot.expressions;

import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionVisitor;
import platform.log.Log;
import platform.log.LogLevel;

public final class Letter extends AbstractExpression{
	public static final Letter E = new Letter('e');
	public static final Letter I = new Letter('i');
	public static final Letter Y = new Letter('y');
	public static final Letter X = new Letter('x');
	public static final Letter Z = new Letter('z');
	// public static final Letter STAR = new Letter('*');
	public static final Letter NOTEXISTS = new Letter('∄');
	public static final Letter FORALL = new Letter('∀');
	public static final Letter UNKNOWN = new Letter('?');

	public Letter(final char letter){
		value = letter;
	}

	private final char value;

	public void toString(final StringBuffer buffer){
		buffer.append(value);
	}

	public INumber value(final IValueList valueList){
		if(value == 'i')
			return Complex.I;
		return valueList.value(value).value();
	}

	public double dvalue(final char letter, final double value){
		if(letter == 'i')
			return 0;
		if(this.value == letter)
			return value;
		return 0;
	}

	public double fdvalue(final char letter, final double value){
		if(this.value == 'i'){
			Log.log(LogLevel.ERROR, "fdvalue called on a L(i) instance");
			return 0;
		}
		if(this.value == letter)
			return value;
		return 0;
	}

	public Expression partialSubstitute(final IValueList valueList){
		if(valueList.contains(value))
			return valueList.value(value);
		return this;
	}

	public Expression partialSubstitute(final char letter, final double value){
		if(this.value == letter)
			return new Dou(value);
		return this;
	}

	public Expression partialSubstitute(final char letter, final Expression value){
		if(this.value == letter)
			return value;
		return this;
	}

	public char getLetter(){
		return value;
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(operation != Operation.DIVISION)
			return super.compatible(elementAt, operation);
		if(elementAt instanceof Power && elementAt.compatible(this, operation))
			return true;
		return super.compatible(elementAt, operation);
	}

	public Expression divide(final Expression arg){
		if(arg instanceof Power && arg.compatible(this, Operation.DIVISION))
			return arg.divide(this).inverse();
		return super.divide(arg);
	}

	public boolean hasLetter(final char letter){
		return value == letter;
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("L(");
		buffer.append(value);
		buffer.append(')');
	}

	public boolean isFullDouble(){
		return value != 'i';
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof Letter))
			return false;
		final Letter oth = (Letter)other;
		return value == oth.value;
	}

	public Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append(value);
	}

	public boolean toStringStartsWith(char prefix){
		return value == prefix;
	}
}
