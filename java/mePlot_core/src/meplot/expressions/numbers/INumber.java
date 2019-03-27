package meplot.expressions.numbers;

import meplot.expressions.geometry.ITensor;

public interface INumber extends ITensor{
	IReal real();

	double toDouble();

	IComplex toComplex();

	INumber add(INumber arg);

	INumber multiply(INumber arg);

	INumber divide(INumber arg);

	boolean lessThan(INumber rightVal);

	boolean greaterThan(INumber rightVal);

	boolean isAlmostEqual(INumber rightVal);

	INumber ininverse();

	boolean isReal();

	INumber inopposite();

	INumber insquare();

	float toFloat();

	double norm();
}
