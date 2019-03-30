package meplot.graphics.plotters;

import meplot.graphics.graphs.Graph;
import platform.lists.IIterator;
import platform.lists.List;

public interface IPlotterSelector {
	void setCleanContour(boolean clean);

	int[][] getZBuffer();

	boolean isFreeroamMode();

	void next3DMode();

	void previous3DMode();

	void setMode(List<Graph> graphList);

	boolean isAxisFirstMode(IIterator<Graph> subIterator);

	Plotter selectPlotter(Graph graph);

	boolean isAxisLaterMode();

	void disposePlotters();

	void set3Ode(boolean value);
}
