package meplot.graphics.graphs;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.list.IValueList;

public class ThreeParametricGraph extends ParametricGraph{
	private final Expression zexpr;

	public ThreeParametricGraph(final ISubstitutible xfunc, final ISubstitutible yfunc,
			final ISubstitutible zfunc, final int color){
		super(xfunc, yfunc, color);

		zexpr = clean(zfunc).applyConstants();
	}

	public final IValue getFz(){
		return zexpr;
	}

	public final Graph partialSubstitute(final IValueList assumed){
		return new ThreeParametricGraph(getXexpr().partialSubstitute(assumed), getYexpr()
				.partialSubstitute(assumed), zexpr.partialSubstitute(assumed), getColor());
	}
}
