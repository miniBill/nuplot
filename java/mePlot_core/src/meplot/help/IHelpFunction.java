package meplot.help;

import meplot.expressions.functions.IFunction;

public interface IHelpFunction extends IFunction{
	String argumentDescription(int index);

	String argumentName(int index);

	String getDescription();
}
