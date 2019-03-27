package meplot.graphics.plotters;

import meplot.expressions.Expression;
import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.NormalGraph;

public abstract class ExpressionPlotter extends NormalPlotter{
	public final void plot(final NormalGraph graph, final IGraphics graphics,
			final int delta, final IDrawController controller) throws DrawException{
		plot(graph.getExpression(), graphics, delta, controller);
	}

	public abstract void plot(Expression expr, IGraphics graphics, int delta,
			IDrawController controller) throws DrawException;
}
