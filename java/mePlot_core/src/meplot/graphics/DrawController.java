package meplot.graphics;

import meplot.graphics.graphs.Graph;
import meplot.graphics.plotters.*;
import meplot.persistence.Settings;
import platform.lists.IIterator;
import platform.lists.List;
import platform.log.Log;
import platform.log.LogLevel;
import platform.persistence.Persistence;
import platform.persistence.listeners.IntSettingsListener;

public final class DrawController implements IDrawController, Runnable, IntSettingsListener {

	private static void drawEndline(final IGraphics graphics) {
		graphics.setColor(0xFF0000);
		graphics.drawLine(1, 1, 1, 1);
		graphics.setColor(0x00FF00);
		graphics.drawLine(1, 2, 1, 2);
		graphics.setColor(0x0000FF);
		graphics.drawLine(1, 3, 1, 3);
		graphics.setColor(0);
	}

	private Plotter currentPlotter;
	private volatile boolean drawing;

	private final DrawingInfo drawingInfo = new DrawingInfo();

	private int height;

	private Thread last;

	private double maxx;
	private double maxy;
	private double maxz;

	private double minx;
	private double miny;
	private double minz;

	private int phi = -30;
	private final double scale = 1.0;

	private static final IPlotterSelectorFactory selectorFactory = PlotterSelector::new;
	private final IPlotterSelector selector = selectorFactory.getPlotterSelector();

	private int theta = 30;

	private int width;

	public DrawController() {
		Persistence.registerListener(this);
		selector.setCleanContour(Persistence.loadInt(Settings.CLEANCONTOUR) == 1);
		resetPosition();
		PlotterUtil.setPhi(phi);
		PlotterUtil.setTheta(theta);
	}

	public void changedSetting(final String name, final int arg) {
		if (name.equals(Settings.CLEANCONTOUR))
			selector.setCleanContour(arg != 0);
	}

	public Thread doPaint(final IGraphics graphics, final List<Graph> graphList, final int width, final int height) {
		reinit(width, height, graphList);

		final IIterator<Graph> iterator = graphList.getIterator();
		return plot(graphics, iterator);
	}

	public double getMaxx() {
		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		return maxx + 0 + deltaX;
	}

	public double getMaxy() {
		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		return maxy + 0 + deltaY;
	}

	public double getMaxz() {
		return maxz;
	}

	public double getMinx() {
		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		return minx + 0 - deltaX;
	}

	public double getMiny() {
		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		return miny + 0 - deltaY;
	}

	public double getMinz() {
		return minz;
	}

	public int[][] getZBuffer() {
		return selector.getZBuffer();
	}

	public boolean isDrawing() {
		return drawing;
	}

	private Thread plot(final IGraphics graphics, final IIterator<Graph> iterator) {
		drawingInfo.setGraphics(graphics);
		drawingInfo.setIterator(iterator);
		drawingInfo.setThread(new Thread(this));
		drawing = true;
		drawingInfo.getThread().start();
		last = drawingInfo.getThread();
		return drawingInfo.getThread();
	}

	private void reinit(final int width, final int height, final List<Graph> graphList) {
		if (width != this.width || height != this.height) {
			this.width = width;
			this.height = height;
			setOptimalRangeValues();
		}

		selector.setMode(graphList);

		stopDrawingAndJoin();

		PlotterUtil.setSize(width, height);
		PlotterUtil.setPhi(phi);
		PlotterUtil.setTheta(theta);
	}

	public void resetPosition() {
		minx = -5;
		maxx = 5;
		maxy = 5;
		miny = -5;
		minz = -5;
		maxz = 5;
		setOptimalRangeValues();
	}

	public void run() {
		final IGraphics graphics = drawingInfo.getGraphics();

		int initialDt = 32;
		int minDt = 1;
		if (width * height > 0)
			for (int dt = initialDt; drawing && dt >= minDt; dt >>= 1) {
				graphics.setColor(0);
				graphics.fillRect(0, 0, width, height);
				if (selector.isAxisFirstMode(drawingInfo.getIterator().subIterator()))
					AxisPlotter.drawAxis(graphics, this);
				final IIterator<Graph> iterator = drawingInfo.getIterator().subIterator();

				while (drawing && iterator.hasNext()) {
					final Graph graph = iterator.next();

					final int color = graph.getColor();
					graphics.setColor(color);
					final Plotter plotter = selector.selectPlotter(graph);
					currentPlotter = plotter;

					try {
						plotter.plot(graph, graphics, dt, this);
					} catch (final DrawException e) {
						dt = minDt;
						break;
					}
				}
				if (selector.isAxisLaterMode())
					AxisPlotter.drawAxis(graphics, this);

				if (dt == minDt) {
					drawEndline(graphics);
				}
				graphics.flushGraphics();
			}

		Log.log(LogLevel.DEBUG, "Drawing done");

		if (!drawing)
			return;

		drawing = false;
		currentPlotter = null;
	}

	public void set3Ode(final boolean value) {
		selector.set3Ode(value);
	}

	private void setOptimalRangeValues() {
		if (height * width == 0)
			return;
		final double halfDeltax = (maxx - minx) / 2.0;
		final double halfDeltay = (maxy - miny) / 2.0;
		final double hdmax = halfDeltax > halfDeltay ? halfDeltax : halfDeltay;
		final double centerX = (maxx + minx) / 2.0;
		final double centerY = (maxy + miny) / 2.0;
		if (height > width) {
			final double hdmin = hdmax * width / height;
			minx = centerX - hdmin;
			maxx = centerX + hdmin;
			miny = centerY - hdmax;
			maxy = centerY + hdmax;
		} else {
			final double hdmin = hdmax * height / width;
			minx = centerX - hdmax;
			maxx = centerX + hdmax;
			miny = centerY - hdmin;
			maxy = centerY + hdmin;
		}
	}

	private void stopDrawingAndJoin() {
		drawing = false;

		if (last != null)
			try {
				last.join();
			} catch (final InterruptedException e) {
			}
	}

}
