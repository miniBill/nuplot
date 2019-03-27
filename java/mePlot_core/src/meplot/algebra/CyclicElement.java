package meplot.algebra;

import meplot.expressions.numbers.AbstractInteger;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.IReal;
import meplot.expressions.operations.OperationsMath;

// TODO from UCDetector: Class "CyclicElement" is only called from tests
public final class CyclicElement extends AbstractInteger{ // NO_UCD
	private final int field;

	public CyclicElement(final int value, final int elementField){
		super(value);
		field = elementField;
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("CE[");
		buffer.append(getValue());
		buffer.append(',');
		buffer.append(field);
		buffer.append(']');
	}

	public IReal add(final IReal arg){
		if(arg instanceof CyclicElement)
			return add((CyclicElement)arg);
		throw new CyclicException("Cyclic added to noncyclic");
	}

	public CyclicElement add(final CyclicElement arg){
		if(arg.field == field)
			return new CyclicElement((getValue() + arg.getValue()) % field, field);
		throw new CyclicException("Cyclic of different fields added");
	}

	public IReal multiply(final IReal arg){
		if(arg instanceof CyclicElement)
			return multiply((CyclicElement)arg);
		throw new CyclicException("Cyclic multiplied by noncyclic");
	}

	public CyclicElement multiply(final CyclicElement arg){
		if(arg.field == field)
			return new CyclicElement(getValue() * arg.getValue() % field, field);
		throw new CyclicException("Cyclic of different fields multiplied");
	}

	public IReal divide(final IReal arg){
		if(arg instanceof CyclicElement)
			return divide((CyclicElement)arg);
		throw new CyclicException("Cyclic divided by noncyclic");
	}

	public CyclicElement divide(final CyclicElement arg){
		if(arg.field == field)
			return multiply(arg.cinverse());
		throw new CyclicException("Cyclic of different fields divided");
	}

	public CyclicElement cinverse(){
		final int res = OperationsMath.modpow(getValue(), field - 2, field);
		return new CyclicElement(res, field);
	}

	public IReal iropposite(){
		return copposite();
	}

	public CyclicElement copposite(){
		if(getValue() == 0)
			return this;
		return new CyclicElement(field - getValue(), field);
	}

	public void toString(final StringBuffer buffer){
		buffer.append(toFullString());
	}

	public IInt iiopposite(){
		return copposite();
	}

	public IInt isquare(){
		return new CyclicElement(getValue() * getValue() % field, field);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append(getValue());
		buffer.append('_');
		buffer.append(field);
	}
}
