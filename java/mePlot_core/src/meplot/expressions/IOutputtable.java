package meplot.expressions;

import platform.lists.IToString;

public interface IOutputtable extends IToString{
	boolean needParenthesis();

	void toFullString(StringBuilder buffer);

	void toPString(StringBuilder buffer);

	void toHtml(StringBuilder buffer);

	void toWrappedHtml(StringBuilder buffer);

	String toCleanString();

	String toFullString();
}
