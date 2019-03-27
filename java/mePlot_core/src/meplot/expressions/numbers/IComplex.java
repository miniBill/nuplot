package meplot.expressions.numbers;

import meplot.expressions.Expression;

public interface IComplex extends INumber{
	double arg();

	IComplex divide(IReal arg);

	IComplex icadd(INumber arg);

	IComplex icinverse();

	IReal immaginary();

	Expression invdivide(Expression numeratorS);

	IReal real();
}
