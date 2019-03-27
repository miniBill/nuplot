package meplot.expressions.functions.operations;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionCategory;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;
import meplot.help.IHelpFunction;

public final class Mod extends NonsymbolicFunction implements IHelpFunction{
	public Mod(final Expression[] expr){
		super(expr);
	}

	public Mod(final Expression arg0, final Expression arg1){
		super(new Expression[]{arg0, arg1});
	}

	public IFunction fill(final Expression[] args){
		return new Mod(args);
	}

	public String getName(){
		return "mod";
	}

	public double fdvalue(final double[] arg){
		return arg[0] % arg[1];
	}

	protected double dvalue(final INumber[] arg){
		return arg[0].toDouble() % arg[1].toDouble();
	}

	protected INumber value(final INumber[] arg){
		if(arg[0] instanceof Int && arg[1] instanceof Int)
			return intMod((Int)arg[0], (Int)arg[1]);
		return new Dou(dvalue(arg));
	}

	private static INumber intMod(final Int int1, final Int int2){
		final int value = int1.getValue();
		final int mod = int2.getValue();
		return new Int(value % mod);
	}

	protected Expression innerSimplify(final Expression[] vals){
		if(vals[0] instanceof INumber && vals[1] instanceof INumber)
			return value(new INumber[]{(INumber)vals[0], (INumber)vals[1]});
		return fill(vals);
	}

	public int needs(){
		return 2;
	}

	public String getCategory(){
		return FunctionCategory.OPERATIONS;
	}

	public String argumentName(final int index){
		return null;
	}

	public String argumentDescription(final int index){
		return null;
	}

	public String getDescription(){
		return "Returns the remainder of x/y";
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
