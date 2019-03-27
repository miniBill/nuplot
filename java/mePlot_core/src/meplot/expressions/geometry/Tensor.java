package meplot.expressions.geometry;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;

public abstract class Tensor extends AbstractExpression implements ITensor{
	public final Expression add(final Expression other){
		if(isZero())
			return other;
		if(other instanceof ITensor){
			final ITensor tadd = add((ITensor)other);
			if(tadd != null)
				return tadd;
		}
		return gadd(other);
	}

	protected Expression gadd(final Expression other){
		return super.add(other);
	}

	public final Expression multiply(final Expression other){
		if(isOne())
			return other;
		if(isZero())
			return Int.ZERO;
		if(other instanceof ITensor)
			return multiply((ITensor)other);
		return gmultiply(other);
	}

	protected Expression gmultiply(final Expression other){
		return super.multiply(other);
	}

	public final Expression divide(final Expression other){
		if(isZero())
			return Int.ZERO;
		if(other instanceof ITensor)
			return divide((ITensor)other);
		return gdivide(other);
	}

	protected Expression gdivide(final Expression other){
		return super.divide(other);
	}

	public final Expression inverse(){
		return itinverse();
	}

	/**
	 * Adds two Tensor object, and returns a new one containing the result.
	 *
	 * @see Expression#add(Expression)
	 * @param other
	 *            The other Tensor.
	 * @return The result
	 */
	public abstract ITensor add(final ITensor other);

	public abstract ITensor divide(final ITensor other);

	public abstract ITensor multiply(final ITensor other);

	public abstract ITensor itinverse();

	public abstract ITensor matrixDvalue(final IValueList valueList);

	public final Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}
}
