package meplot.graphics.plotters;

import meplot.graphics.graphs.Graph;
import platform.lists.IIterator;
import platform.lists.List;

public interface IPlotterSelector {
	void setCleanContour(boolean clean);

	boolean isFreeroamMode();

	void next3DMode();

	void previous3DMode();

	void setMode(List<Graph> graphList);

	boolean isAxisFirstMode(IIterator<Graph> subIterator);

    boolean isAxisLaterMode();

	void set3Ode(boolean value);
}
