package meplot.graphics.graphs;

import meplot.expressions.Expression;
import meplot.expressions.functions.ode.GenericOde;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.Complex;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.numerical.RungeKutta;

public class OdeGraph extends NormalGraph{
	public OdeGraph(final Expression ode, final int color){
		super(SimplificationHelper.simplify(ode), color);
	}

	public final boolean is3D(){
		final RungeKutta rungeKutta = getRK();
		if(rungeKutta == null)
			return false;
		final Expression x0 = SimplificationHelper.simplify(rungeKutta.next());
		if(x0 instanceof Matrix){
			final Matrix mat = (Matrix)x0;
			return mat.getCols() * mat.getRows() == 3;
		}
		return false;
	}

	public final RungeKutta getRK(){
		final Expression expr = getExpression();
		if(expr instanceof GenericOde){
			final GenericOde ode = (GenericOde)expr;
			return ode.getRK4();
		}
		return null;
	}

	public final boolean is2D(){
		final RungeKutta rungeKutta = getRK();
		if(rungeKutta == null)
			return false;
		final Expression x0 = SimplificationHelper.simplify(rungeKutta.next());
		if(x0 instanceof Matrix){
			final Matrix mat = (Matrix)x0;
			return mat.getCols() * mat.getRows() == 2;
		}
		return x0 instanceof Complex;
	}
}
