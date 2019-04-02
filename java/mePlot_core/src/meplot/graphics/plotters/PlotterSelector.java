package meplot.graphics.plotters;

import meplot.graphics.DrawMode;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.OdeGraph;
import meplot.graphics.graphs.ThreeParametricGraph;
import platform.lists.IIterator;
import platform.lists.List;

public class PlotterSelector implements IPlotterSelector {
	private static final int THREED = 0;
	public static final int CONTOUR = 1;
	public static final int GRAYCONTOUR = 2;
	private static final int FIELD = 3;
	private static final int COMPLEX = 4;
	private static final int MAXMODE = 4;

	protected static DrawMode getMode(final Graph graph) {
		if (graph == null)
			return DrawMode.MODE2D;

		if (graph instanceof OdeGraph)
			return DrawMode.MODEODE;

		if (graph instanceof ThreeParametricGraph)
			return DrawMode.MODE3PARAM;

		if (graph.is3D()) {
			if (graph.isImplicit())
				return DrawMode.MODE3IMPLICIT;
			return DrawMode.MODE3D;
		}

		return DrawMode.MODE2D;
	}

	private DrawMode mode = DrawMode.MODE2D;
	private int threeDMode = THREED;
	private boolean is3Ode;
	private boolean cleanContour;

	public final boolean isAxisFirstMode(final IIterator<Graph> iterator) {
		return mode.equals(DrawMode.MODE2D)
				|| mode.equals(DrawMode.MODE3D) && (threeDMode == FIELD || threeDMode == COMPLEX
						|| cleanContour && (threeDMode == CONTOUR || threeDMode == GRAYCONTOUR))
				|| mode.equals(DrawMode.MODEODE) && !iterator.next().is3D();
	}

	public boolean isAxisLaterMode() {
		return mode.equals(DrawMode.MODE3D) && (threeDMode == GRAYCONTOUR || threeDMode == CONTOUR);
	}

	public boolean isFreeroamMode() {
		return mode.equals(DrawMode.MODE3D) && threeDMode == THREED || mode.equals(DrawMode.MODE3PARAM)
				|| mode.equals(DrawMode.MODE3IMPLICIT) || is3Ode;
	}

	public final void next3DMode() {
		threeDMode++;
		if (threeDMode > MAXMODE)
			threeDMode = 0;
	}

	public final void previous3DMode() {
		threeDMode--;
		if (threeDMode < 0)
			threeDMode = MAXMODE;
	}

	public final void set3Ode(final boolean value) {
		is3Ode = value;
	}

	public void setCleanContour(final boolean value) {
		cleanContour = value;
	}

	public void setMode(final List<Graph> graphList) {
		if (graphList == null)
			return;
		if (graphList.isEmpty())
			mode = DrawMode.MODE2D;
		else {
			final Graph graph = graphList.getFirst();
			mode = getMode(graph);
		}
	}
}
