package meplot.expressions;

import meplot.expressions.geometry.ITensor;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.IDou;
import meplot.expressions.numbers.INumber;

public interface IValue{
	double dvalue();

	double dvalue(char letter, double value);

	double fdvalue(char letter, double value);

	IDou idouvalue(char letter, double value);

	IDou idouvalue(IValueList valueList);

	boolean isFullDouble();

	ITensor matrixDvalue(char letter, double value);

	ITensor matrixDvalue(IValueList valueList);

	INumber value();

	INumber value(char letter, double value);

	INumber value(IValueList arg);
}
