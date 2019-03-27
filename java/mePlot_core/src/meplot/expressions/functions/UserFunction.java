package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.INumber;

public final class UserFunction extends ExpandFunction{
	private final int needed;
	private final String name;
	private final char[] vars;
	private final Expression expr;

	public UserFunction(final int needed, final String name, final char[] vars,
			final Expression value){
		super(new Expression[needed]);
		this.needed = needed;
		this.name = name;
		this.vars = vars;
		expr = value;
	}

	private UserFunction(final int needed, final String name, final char[] vars,
			final Expression value, final Expression[] expr){
		super(expr);
		this.needed = needed;
		this.name = name;
		this.vars = vars;
		this.expr = value;
	}

	public IFunction fill(final Expression[] args){
		return new UserFunction(needed, name, vars, expr, args);
	}

	public String getName(){
		return name;
	}

	public int needs(){
		return needed;
	}

	protected double dvalue(final INumber[] args){
		final IValueList valueList = new ValueList();
		for(int i = 0; i < vars.length; i++)
			valueList.add(vars[i], args[i]);
		return expr.partialSubstitute(valueList).dvalue();
	}

	protected INumber value(final INumber[] args){
		final IValueList valueList = new ValueList();
		for(int i = 0; i < vars.length; i++)
			valueList.add(vars[i], args[i]);
		return expr.value(valueList);
	}

	public String getCategory(){
		return FunctionCategory.USER_DEFINED;
	}

	protected Expression expand(final Expression[] args){
		final IValueList valueList = new ValueList();
		for(int i = 0; i < vars.length; i++)
			valueList.add(vars[i], args[i]);
		return expr.partialSubstitute(valueList);
	}

	public double fdvalue(final char letter, final double value){
		return expand(getArguments()).fdvalue(letter, value);
	}

	public boolean isFullDouble(){
		return expr.isFullDouble();
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		return expand(getArguments()).fdvalue(letter, value);
	}
}
