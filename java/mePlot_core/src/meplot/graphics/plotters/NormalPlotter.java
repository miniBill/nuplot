package meplot.graphics.plotters;

import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.NormalGraph;

public abstract class NormalPlotter implements Plotter{
	public final void plot(final Graph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException{
		if(PlotterUtil.getWidth() * PlotterUtil.getHeight() == 0)
			return;
		if(graph instanceof NormalGraph)
			plot((NormalGraph)graph, graphics, delta, controller);
	}

	public abstract void plot(final NormalGraph graph, final IGraphics graphics,
			final int delta, final IDrawController controller) throws DrawException;
}
