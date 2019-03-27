package meplot.expressions.numbers;

import meplot.expressions.list.IValueList;

public interface IReal extends INumber{
	double EQUAL_THRESOLD = 0.01;

	IReal iropposite();

	IReal add(IReal real);

	IReal multiply(IReal real);

	boolean isNegative();

	IReal rsquare();

	IReal irSimplify();

	IDou douvalue(IValueList valueList);

	boolean isPositive();

	boolean isInt();

	IReal irinverse();

	IInt intify();

	IReal abs();
}
