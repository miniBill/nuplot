package meplot.graphics.graphs;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.list.IValueList;
import meplot.expressions.operations.BooleanOp;

public class ParametricGraph extends AbstractGraph{
	private final Expression xexpr;
	private final Expression yexpr;

	protected final ISubstitutible getXexpr(){
		return xexpr;
	}

	protected final ISubstitutible getYexpr(){
		return yexpr;
	}

	public ParametricGraph(final ISubstitutible xfunc, final ISubstitutible yfunc,
			final int color){
		super(color);

		xexpr = clean(xfunc).applyConstants();
		yexpr = clean(yfunc).applyConstants();
	}

	protected static final ISubstitutible clean(final ISubstitutible expr){
		if(expr instanceof BooleanOp){
			final BooleanOp bexpr = (BooleanOp)expr;
			return bexpr.getRight();
		}
		return expr;
	}

	public final IValue getFx(){
		return xexpr;
	}

	public final IValue getFy(){
		return yexpr;
	}

	public Graph partialSubstitute(final IValueList assumed){
		return new ParametricGraph(xexpr.partialSubstitute(assumed),
				yexpr.partialSubstitute(assumed), getColor());
	}

	public final boolean isRadial(){
		return false;
	}

	public final boolean is3D(){
		return false;
	}

	public final boolean isImplicit(){
		return false;
	}
}
