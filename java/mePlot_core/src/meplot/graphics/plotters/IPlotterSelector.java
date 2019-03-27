package meplot.graphics.plotters;

import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.GraphIterator;
import meplot.graphics.graphs.GraphList;

public interface IPlotterSelector{
	void setCleanContour(boolean clean);

	int[][] getZBuffer();

	boolean isFreeroamMode();

	void next3DMode();

	void previous3DMode();

	void setMode(GraphList graphList);

	boolean isAxisFirstMode(GraphIterator subIterator);

	Plotter selectPlotter(Graph graph);

	boolean isAxisLaterMode();

	void disposePlotters();

	void set3Ode(boolean value);
}
