package meplot.expressions.functions.algebra;

import meplot.expressions.Expression;
import meplot.expressions.ISimplifiable;
import meplot.expressions.functions.ArbitraryFunction;
import meplot.expressions.functions.IFunction;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import platform.lists.IIterable;

public final class Span extends ArbitraryFunction{
	public Span(final Expression[] values){
		super(values, values == null ? new boolean[0] : allTrue(values.length));
	}

	protected Expression innerStepSimplify(final Expression[] vals){
		final IExpressionList toret = new ExpressionList();
        for (Expression val : vals)
			if (!IIterable.contains(toret, val))
				toret.add(val);
		for(int c = 0; c < vals.length; c++)
			if(vals[c].isSimplified())
				for(int d = c; d < vals.length; d++)
					if(vals[d].isSimplified()){
						final Expression mul = SimplificationHelper.simplify(vals[c].multiply(vals[d]));
                        if(!IIterable.contains(toret, mul)){
							toret.add(mul); // TODO: Fix
                            if(IIterable.length(toret) > 10)
								throw new RuntimeException();
						}
					}
        if(IIterable.length(toret) == vals.length && areSimplified(vals))
			return new Matrix(vals);
		return new Span(toret.toArray());
	}

	private static boolean areSimplified(final ISimplifiable[] vals){
		for (ISimplifiable val : vals)
			if (!val.isSimplified())
				return false;
		return true;
	}

	protected Expression innerSimplify(final Expression[] vals){
		return innerStepSimplify(vals);
	}

	public IFunction fill(final Expression[] args){
		return new Span(args);
	}

    public String getName(){
		return "span";
	}

	protected double dvalue(final INumber[] arg){
		return 0;
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		return 0;
	}

	protected INumber value(final INumber[] topass){
		return Int.ZERO;
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
