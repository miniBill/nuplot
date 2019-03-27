package meplot.android.paid.gui;

import meplot.graphics.DrawMode;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.NormalGraph;
import meplot.graphics.plotters.Plotter;
import meplot.graphics.plotters.PlotterSelector;

public final class PaidSelector extends PlotterSelector{
	@Override
	protected Plotter innerSelectPlotter(Graph graph){
		if(graph instanceof NormalGraph && getMode(graph).equals(DrawMode.MODE2D) && graph.isImplicit()
				&& ((NormalGraph)graph).isDisequation())
			return contourPlotter;

		return super.innerSelectPlotter(graph);
	}
}
