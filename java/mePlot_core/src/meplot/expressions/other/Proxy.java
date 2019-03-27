package meplot.expressions.other;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionVisitor;

public class Proxy extends AbstractExpression{
	private final Expression value;

	protected Proxy(final Expression val){
		value = val;
	}

	public boolean hasLetter(final char letter){
		return value.hasLetter(letter);
	}

	public void toFullString(final StringBuffer buffer){
		value.toFullString(buffer);
	}

	public INumber value(final IValueList letters){
		return value.value(letters);
	}

	public Expression add(final Expression other){
		return value.add(other);
	}

	public boolean compatible(final Expression elementAt, final char operation){
		return value.compatible(elementAt, operation);
	}

	public boolean containsMatrix(){
		return value.containsMatrix();
	}

	public Expression divide(final Expression divisor){
		return value.divide(divisor);
	}

	public double dvalue(final char letter, final double value){
		return this.value.dvalue(letter, value);
	}

	public boolean equals(final Object obj){
		return value.equals(obj);
	}

	public int hashCode(){
		return value.hashCode();
	}

	public Expression innerSimplify(){
		return value.innerSimplify();
	}

	public Expression innerStepSimplify(){
		return value.innerStepSimplify();
	}

	public Expression inverse(){
		return value.inverse();
	}

	public boolean isOne(){
		return value.isOne();
	}

	public boolean isZero(){
		return value.isZero();
	}

	public Expression multiply(final Expression other){
		return value.multiply(other);
	}

	public boolean needParenthesis(){
		return value.needParenthesis();
	}

	public Expression opposite(){
		return value.opposite();
	}

	public Expression partialSubstitute(final IValueList valueList){
		return value.partialSubstitute(valueList);
	}

	public Expression square(){
		return value.square();
	}

	public void toPString(final StringBuffer buffer){
		value.toPString(buffer);
	}

	public void toString(final StringBuffer buffer){
		value.toString(buffer);
	}

	public INumber value(final char var, final double val){
		return value.value(var, val);
	}

	public double fdvalue(final char letter, final double value){
		return this.value.fdvalue(letter, value);
	}

	public boolean isFullDouble(){
		return value.isFullDouble();
	}

	public Expression partialSubstitute(final char letter, final double value){
		return this.value.partialSubstitute(letter, value);
	}

	public Expression partialSubstitute(final char letter, final Expression value){
		return this.value.partialSubstitute(letter, value);
	}

	public boolean isIdentical(final Expression other){
		if(other instanceof Proxy)
			return ((Proxy)other).value.isIdentical(value);
		return false;
	}

	public Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}

	public Expression getValue(){
		return value;
	}

	public void toHtml(final StringBuffer buffer){
		value.toHtml(buffer);
	}
}
