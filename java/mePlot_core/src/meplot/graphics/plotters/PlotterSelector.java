package meplot.graphics.plotters;

import meplot.graphics.DrawMode;
import meplot.graphics.IDisposable;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.NormalGraph;
import meplot.graphics.graphs.OdeGraph;
import meplot.graphics.graphs.ParametricGraph;
import meplot.graphics.graphs.ThreeParametricGraph;
import meplot.graphics.plotters.three.ComplexPlotter;
import meplot.graphics.plotters.three.ContourPlotter;
import meplot.graphics.plotters.three.FieldPlotter;
import meplot.graphics.plotters.three.OdePlotter;
import meplot.graphics.plotters.three.ThreeParametricPlotter;
import meplot.graphics.plotters.three.ThreePlotter;
import meplot.graphics.plotters.three.ThreeScanner;
import platform.lists.IIterator;
import platform.lists.List;
import platform.log.Log;
import platform.log.LogLevel;

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

	private Plotter[] plotters = new Plotter[0];

	private final Plotter complexPlotter = new ComplexPlotter();
	protected final ContourPlotter contourPlotter = new ContourPlotter();
	private final Plotter fieldPlotter = new FieldPlotter();
	private final Plotter odePlotter = new OdePlotter();
	private final Plotter parametricPlotter = new ParametricPlotter();
	private final Plotter radialPlotter = new RadialPlotter();
	protected final SingleScanner singleScanner = new SingleScanner();
	private final Plotter threeParamPlotter = new ThreeParametricPlotter();
	private final ThreePlotter threePlotter = new ThreePlotter();
	private final ThreeScanner threeScanner = new ThreeScanner();
	protected final Plotter xyScanner = new XyScanner();

	public final void disposePlotters() {
		if (plotters != null)
			for (int c = 0; c < plotters.length; c++)
				if (plotters[c] instanceof IDisposable)
					((IDisposable) plotters[c]).dispose();
	}

	public int[][] getZBuffer() {
		return threePlotter.getZBuffer();
	}

	private void initPlotters() {
		plotters = new Plotter[MAXMODE + 1];
		plotters[GRAYCONTOUR] = contourPlotter;
		plotters[CONTOUR] = contourPlotter;
		plotters[THREED] = threePlotter;
		plotters[FIELD] = fieldPlotter;
		plotters[COMPLEX] = complexPlotter;
	}

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

	public final Plotter selectPlotter(final Graph graph) {
		if (plotters.length == 0)
			initPlotters();

		return innerSelectPlotter(graph);
	}

	protected Plotter innerSelectPlotter(final Graph graph) {
		if (graph instanceof ParametricGraph) {
			if (graph instanceof ThreeParametricGraph)
				return threeParamPlotter;
			return parametricPlotter;
		}

		// should always be true, but it's better to be robust
		if (graph instanceof NormalGraph) {

			if (graph instanceof OdeGraph)
				return odePlotter;

			if (graph.isRadial())
				return radialPlotter;

			final DrawMode gmode = getMode(graph);

			if (gmode.equals(DrawMode.MODE2D)) {
				if (graph.isImplicit())
					return xyScanner;
				return singleScanner;
			}

			if (gmode.equals(DrawMode.MODE3IMPLICIT))
				return threeScanner;

			if (threeDMode == CONTOUR || threeDMode == GRAYCONTOUR)
				contourPlotter.setMode(threeDMode, cleanContour);

			if (threeDMode < plotters.length)
				return plotters[threeDMode];
		}

		Log.log(LogLevel.ERROR, "Couldn't select plotter! Falling back to default.");
		return singleScanner;
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
