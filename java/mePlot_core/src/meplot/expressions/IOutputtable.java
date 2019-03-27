package meplot.expressions;

import platform.lists.IToString;

public interface IOutputtable extends IToString{
	boolean needParenthesis();

	void toFullString(StringBuffer buffer);

	void toPString(StringBuffer buffer);

	void toHtml(StringBuffer buffer);

	void toWrappedHtml(StringBuffer buffer);

	String toCleanString();

	String toFullString();
}
