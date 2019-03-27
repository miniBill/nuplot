package meplot.graphics.plotters;

import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.ParametricGraph;

public abstract class AbstractParametricPlotter implements Plotter{
	public final void plot(final Graph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException{
		if(PlotterUtil.getWidth() * PlotterUtil.getHeight() == 0)
			return;
		if(graph instanceof ParametricGraph)
			plot((ParametricGraph)graph, graphics, delta, controller);
	}

	protected abstract void plot(final ParametricGraph graph, final IGraphics graphics,
			final int delta, final IDrawController controller) throws DrawException;
}
