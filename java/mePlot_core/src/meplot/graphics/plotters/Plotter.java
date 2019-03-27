package meplot.graphics.plotters;

import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.Graph;

public interface Plotter{
	void plot(final Graph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException;

	int getProgress();
}
