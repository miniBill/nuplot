package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;

public final class InvisibleHold extends Hold{
	public InvisibleHold(Expression value){
		super(value);
	}

	public String getName(){
		return "";
	}

	public IFunction fill(Expression expr){
		return new InvisibleHold(expr);
	}

	public void toHtml(StringBuilder buffer){
		getArgument().toHtml(buffer);
	}

	public void toPString(StringBuilder buffer){
		getArgument().toPString(buffer);
	}

	public void toString(StringBuilder buffer){
		getArgument().toString(buffer);
	}
}
