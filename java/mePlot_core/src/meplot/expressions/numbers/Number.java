package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.geometry.Tensor;
import meplot.expressions.list.IValueList;
import meplot.expressions.operations.Operation;
import meplot.expressions.visitors.IExpressionTensorVisitor;

public abstract class Number extends Tensor implements INumber{
	public final boolean hasLetter(final char letter){
		return false;
	}

	public final ITensor add(final ITensor other){
		if(other instanceof INumber)
			return add((INumber)other);
		// BEWARE: this assumes that the other tensors know how to add numbers
		return other.add(this);
	}

	public abstract INumber add(INumber arg);

	public final ITensor multiply(final ITensor other){
		if(isOne())
			return other;
		if(other instanceof INumber)
			return multiply((INumber)other);
		// BEWARE: this assumes that the other tensors know how to multiply
		// numbers
		return other.multiply(this);
	}

	public abstract INumber multiply(final INumber arg);

	public final ITensor divide(final ITensor arg){
		if(arg.isOne())
			return this;
		if(arg instanceof INumber)
			return divide((INumber)arg);
		return multiply(arg.itinverse());
	}

	public abstract INumber divide(final INumber arg);

	public abstract INumber inopposite();

	public final Expression opposite(){
		return inopposite();
	}

	public abstract INumber ininverse();

	public final ITensor itinverse(){
		return ininverse();
	}

	public final INumber value(final IValueList letters){
		return this;
	}

	public abstract boolean isReal();

	public abstract IReal real();

	public final boolean containsMatrix(){
		return false;
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(operation == Operation.ADDITION)
			return isZero();
		if(operation == Operation.MULTIPLICATION)
			return isZero() || isOne();
		return super.compatible(elementAt, operation);
	}

	public final boolean lessThan(final INumber arg){
		if(arg instanceof Number)
			return lessThan((Number)arg);
		return !arg.greaterThan(this) && !arg.isAlmostEqual(this);
	}

	public abstract boolean lessThan(Number arg);

	public final boolean greaterThan(final INumber arg){
		if(arg instanceof Number)
			return greaterThan((Number)arg);
		return !arg.lessThan(this) && !arg.isAlmostEqual(this);
	}

	public abstract boolean greaterThan(Number arg);

	public abstract double norm();

	public abstract IComplex toComplex();

	public final boolean isAlmostEqual(final INumber arg){
		if(arg instanceof Number)
			return isAlmostEqual((Number)arg);
		return arg.isAlmostEqual(this);
	}

	public abstract boolean isAlmostEqual(Number rightVal);

	public abstract double toDouble();

	public final double fdvalue(final char letter, final double value){
		return toDouble();
	}

	public final ITensor matrixDvalue(final IValueList valueList){
		return nmatrixDvalue(valueList);
	}

	// ESCA-JAVA0173:
	public INumber nmatrixDvalue(final IValueList valueList){
		return this;
	}

	public final Expression partialSubstitute(final char letter, final double value){
		return this;
	}

	public final Expression partialSubstitute(final char var, final Expression expression){
		return this;
	}

	public final Expression partialSubstitute(final IValueList valueList){
		return this;
	}

	public final Expression square(){
		return insquare();
	}

	public final Expression accept(final IExpressionTensorVisitor visitor){
		return visitor.visit(this);
	}
}
