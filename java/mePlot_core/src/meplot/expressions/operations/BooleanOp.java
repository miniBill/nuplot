package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;

/**
 * Boolean expression.
 * 
 * @author Leonardo Taglialegne
 */
public final class BooleanOp extends AbstractExpression{
	/**
	 * Left hand value.
	 */
	private final Expression left;
	/**
	 * Operator.
	 */
	private final char boole;
	/**
	 * Right hand value.
	 */
	private final Expression right;

	public BooleanOp(final Expression left, final char boole, final Expression right){
		this.left = left;
		this.boole = boole;
		this.right = right;
	}

	/**
	 * {@inheritDoc}
	 */
	public void toString(final StringBuffer buffer){
		left.toString(buffer);
		buffer.append(boole);
		right.toString(buffer);
	}

	/**
	 * {@inheritDoc}
	 */
	public INumber value(final IValueList valueList){
		return rvalue(valueList);
	}

	private IInt rvalue(){
		final INumber leftVal = left.value();
		final INumber rightVal = right.value();
		return rvalue(leftVal, rightVal);
	}

	private Letter lvalue(){
		final INumber leftVal = left.value();
		final INumber rightVal = right.value();
		return lvalue(leftVal, rightVal);
	}

	private IInt rvalue(final IValueList valueList){
		final INumber leftVal = left.value(valueList);
		final INumber rightVal = right.value(valueList);
		return rvalue(leftVal, rightVal);
	}

	private IInt rvalue(final INumber leftVal, final INumber rightVal){
		if(Operation.LESS == boole)
			return leftVal.lessThan(rightVal) ? Int.ONE : Int.ZERO;
		if(Operation.GEQ == boole)
			return leftVal.lessThan(rightVal) ? Int.ZERO : Int.ONE;
		if(Operation.GREATER == boole)
			return leftVal.greaterThan(rightVal) ? Int.ONE : Int.ZERO;
		if(Operation.LEQ == boole)
			return leftVal.greaterThan(rightVal) ? Int.ZERO : Int.ONE;
		if(Operation.EQUALS == boole)
			return leftVal.isAlmostEqual(rightVal) ? Int.ONE : Int.ZERO;
		if(Operation.NEQ == boole)
			return leftVal.isAlmostEqual(rightVal) ? Int.ZERO : Int.ONE;
		return Int.ZERO;
	}

	private Letter lvalue(final INumber leftVal, final INumber rightVal){
		if(Operation.LESS == boole)
			return leftVal.lessThan(rightVal) ? Letter.FORALL : Letter.NOTEXISTS;
		if(Operation.GEQ == boole)
			return leftVal.lessThan(rightVal) ? Letter.NOTEXISTS : Letter.FORALL;
		if(Operation.GREATER == boole)
			return leftVal.greaterThan(rightVal) ? Letter.FORALL : Letter.NOTEXISTS;
		if(Operation.LEQ == boole)
			return leftVal.greaterThan(rightVal) ? Letter.NOTEXISTS : Letter.FORALL;
		if(Operation.EQUALS == boole)
			return leftVal.isAlmostEqual(rightVal) ? Letter.FORALL : Letter.NOTEXISTS;
		if(Operation.NEQ == boole)
			return leftVal.isAlmostEqual(rightVal) ? Letter.NOTEXISTS : Letter.FORALL;
		return Letter.UNKNOWN;
	}

	public Expression partialSubstitute(final IValueList valueList){
		final Expression nleft = left.partialSubstitute(valueList);
		final Expression nright = right.partialSubstitute(valueList);
		return new BooleanOp(nleft, boole, nright);
	}

	public Expression partialSubstitute(final char letter, final double value){
		final Expression nleft = left.partialSubstitute(letter, value);
		final Expression nright = right.partialSubstitute(letter, value);
		return new BooleanOp(nleft, boole, nright);
	}

	public Expression partialSubstitute(final char letter, final Expression value){
		final Expression nleft = left.partialSubstitute(letter, value);
		final Expression nright = right.partialSubstitute(letter, value);
		return new BooleanOp(nleft, boole, nright);
	}

	/**
	 * {@inheritDoc} Always returns true for BooleanOp.
	 * 
	 * @return Always true for BooleanOp.
	 */
	public boolean needParenthesis(){
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean hasLetter(final char letter){
		return left.hasLetter(letter) || right.hasLetter(letter);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean containsMatrix(){
		return left.containsMatrix() || right.containsMatrix();
	}

	/**
	 * {@inheritDoc}
	 */
	public void toFullString(final StringBuffer buffer){
		buffer.append(boole);
		buffer.append('(');
		left.toFullString(buffer);
		buffer.append(',');
		right.toFullString(buffer);
		buffer.append(')');
	}

	public boolean isDecided(){
		return left instanceof INumber && right instanceof INumber;
	}

	public boolean isFinallyTrue(){
		return rvalue().isPositive();
	}

	/**
	 * {@inheritDoc}
	 */
	public Expression innerSimplify(){
		if(isDecided())
			return lvalue();
		final Expression nright = right.partialSimplify();
		final Expression nleft = left.partialSimplify();
		if(nright.isIdentical(right) && nleft.isIdentical(left)){
			simplified = true;
			return this;
		}
		return new BooleanOp(nleft, boole, nright);
	}

	/**
	 * {@inheritDoc}
	 */
	public Expression innerStepSimplify(){
		if(isDecided())
			return lvalue();
		final Expression nleft = left.innerStepSimplify();
		final Expression nright = right.innerStepSimplify();
		if(nright.isIdentical(right) && nleft.isIdentical(left)){
			simplified = true;
			return this;
		}
		return new BooleanOp(nleft, boole, nright);
	}

	/**
	 * Returns the operator.
	 * 
	 * @return Operator.
	 */
	public char getBool(){
		return boole;
	}

	/**
	 * Returns the left hand value.
	 * 
	 * @return Left hand value.
	 */
	public Expression getLeft(){
		return left;
	}

	/**
	 * Returns the right hand value.
	 * 
	 * @return Right hand value.
	 */
	public Expression getRight(){
		return right;
	}

	/**
	 * {@inheritDoc}
	 */
	public double fdvalue(final char letter, final double value){
		final double leftVal = left.fdvalue(letter, value);
		final double rightVal = right.fdvalue(letter, value);
		if(Operation.LESS == boole)
			return leftVal < rightVal ? 1 : 0;
		if(Operation.GEQ == boole)
			return leftVal < rightVal ? 1 : 0;
		if(Operation.GREATER == boole)
			return leftVal > rightVal ? 1 : 0;
		if(Operation.LEQ == boole)
			return leftVal <= rightVal ? 1 : 0;
		if(Operation.EQUALS == boole)
			return isAlmostEqual(leftVal, rightVal) ? 1 : 0;
		if(Operation.NEQ == boole)
			return isAlmostEqual(leftVal, rightVal) ? 0 : 1;
		return 0;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean isFullDouble(){
		return left.isFullDouble() && right.isFullDouble();
	}

	private static boolean isAlmostEqual(final double leftVal, final double rightVal){
		if(leftVal == 0)
			return rightVal == 0 || Math.abs((rightVal - leftVal) / rightVal) < IReal.EQUAL_THRESOLD;
		return Math.abs((leftVal - rightVal) / leftVal) < IReal.EQUAL_THRESOLD;
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof BooleanOp))
			return false;
		final BooleanOp oth = (BooleanOp)other;
		return boole == oth.boole && left.isIdentical(oth.left) && right.isIdentical(oth.right);
	}

	public Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}

	public Expression expand(){
		return new BooleanOp(left.expand(), boole, right.expand());
	}

	public void toHtml(final StringBuffer buffer){
		left.toWrappedHtml(buffer);
		buffer.append(boole);
		right.toWrappedHtml(buffer);
	}

	public boolean isDisequation(){
		return boole != Operation.EQUALS && boole != Operation.NEQ;
	}

	public boolean toStringStartsWith(char prefix){
		return left.toStringStartsWith(prefix);
	}
}
