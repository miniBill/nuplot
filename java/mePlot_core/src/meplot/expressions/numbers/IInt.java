package meplot.expressions.numbers;

public interface IInt extends IReal{
	int getValue();

	IInt add(IInt arg);

	IInt multiply(IInt arg);

	IInt iiopposite();

	IInt iiabs();

	boolean isIdentical(IInt other);

	IInt isquare();
}
