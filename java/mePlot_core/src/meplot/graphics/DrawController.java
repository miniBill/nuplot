package meplot.graphics;

import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.GraphIterator;
import meplot.graphics.graphs.GraphList;
import meplot.graphics.plotters.AxisPlotter;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.IPlotterSelector;
import meplot.graphics.plotters.IPlotterSelectorFactory;
import meplot.graphics.plotters.Plotter;
import meplot.graphics.plotters.PlotterSelector;
import meplot.graphics.plotters.PlotterUtil;
import meplot.graphics.plotters.three.ThreePlotter;
import meplot.graphics.plotters.three.ThreeScanner;
import meplot.persistence.Settings;
import platform.Platform;
import platform.log.Log;
import platform.log.LogLevel;
import platform.persistence.Persistence;
import platform.persistence.listeners.IntSettingsListener;

public final class DrawController implements IDrawController, Runnable, IntSettingsListener{
	private static final String COMA = ",";

	/**
	 * Use this value for initial delta if you want 3d stuff to be drawn with
	 * increased delta.
	 */
	public static final int MAGIC_DELTA = 16;

	private static void drawEndline(final IGraphics graphics){
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

	private boolean freeroam = true;

	private int height;

	private int initialDt = 32;

	private boolean invertLR;
	private boolean invertUD;

	private Thread last;

	private boolean lastDrawingWas3D = false;

	private double maxx;
	private double maxy;
	private double maxz;

	private int minDt = 1;

	private double minx;
	private double miny;
	private double minz;

	private double movephi;
	private double movetheta;

	private double movex;
	private double movey;

	private int phi = -30;
	private double scale = 1.0;

	private static IPlotterSelectorFactory selectorFactory = new IPlotterSelectorFactory(){
		public IPlotterSelector getPlotterSelector(){
			return new PlotterSelector();
		}
	};
	private final IPlotterSelector selector = selectorFactory.getPlotterSelector();

	private int theta = 30;

	private int width;

	public DrawController(){
		Persistence.registerListener(this);
		final int arg = Persistence.loadInt(Settings.INVERTCONTROLS);
		invertLR = arg % 2 == 1;
		invertUD = arg > 1;
		selector.setCleanContour(Persistence.loadInt(Settings.CLEANCONTOUR) == 1);
		resetPosition();
		PlotterUtil.setPhi(phi);
		PlotterUtil.setTheta(theta);
	}

	public void changedSetting(final String name, final int arg){
		if(name.equals(Settings.INVERTCONTROLS)){
			invertLR = arg % 2 == 1;
			invertUD = arg > 1;
		}
		if(name.equals(Settings.CLEANCONTOUR))
			selector.setCleanContour(arg != 0);
	}

	public Thread doPaint(final IGraphics graphics, final GraphList graphList, final int width, final int height){
		reinit(width, height, graphList);

		final GraphIterator iterator = graphList.getIterator();
		return plot(graphics, iterator);
	}

	public String getBoundingBoxInfo(){
		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		return scale + "\n" + minx + COMA + maxx + COMA + miny + COMA + maxy + "\n" + deltaX + COMA + deltaY + "\n"
				+ (minx + movex - deltaX) + COMA + (maxx + movex + deltaX) + COMA + (miny + movey - deltaY) + COMA
				+ (maxy + movey + deltaY);
	}

	public DrawingInfo getDrawingInfo(){
		return drawingInfo;
	}

	public double getMaxx(){
		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		return maxx + movex + deltaX;
	}

	public double getMaxy(){
		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		return maxy + movey + deltaY;
	}

	public double getMaxz(){
		return maxz;
	}

	public double getMinx(){
		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		return minx + movex - deltaX;
	}

	public double getMiny(){
		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		return miny + movey - deltaY;
	}

	public double getMinz(){
		return minz;
	}

	public int getProgress(){
		return currentPlotter == null ? 0 : currentPlotter.getProgress();
	}

	public int[][] getZBuffer(){
		return selector.getZBuffer();
	}

	public void goDown(){
		final double sdy = (maxy - miny) / 4.0;
		miny += sdy;
		maxy += sdy;
	}

	public void goLeft(){
		final double ady = (maxy - miny) / 4.0;
		minx -= ady;
		maxx -= ady;
	}

	public void goRight(){
		final double ddy = (maxy - miny) / 4.0;
		minx += ddy;
		maxx += ddy;
	}

	public void goUp(){
		final double wdy = (maxy - miny) / 4.0;
		miny -= wdy;
		maxy -= wdy;
	}

	public boolean isDrawing(){
		return drawing;
	}

	public boolean isFreeroam(){
		return freeroam && isFreeroamMode();
	}

	public boolean isFreeroamMode(){
		return selector.isFreeroamMode();
	}

	public void move(final float startx, final float endx, final float starty, final float endy){
		tempMove(startx, endx, starty, endy);
		if(lastDrawingWas3D){
			phi += movephi;
			theta += movetheta;

			movephi = 0;
			movetheta = 0;
		}
		else{
			minx += movex;
			maxx += movex;

			miny += movey;
			maxy += movey;

			movex = 0;
			movey = 0;
		}
	}

	public void tempMove(final float startx, final float currx, final float starty, final float curry){
		if(lastDrawingWas3D){
			final double nstartx = 2.0 * startx / width - 0.5;
			final double ncurrx = 2.0 * currx / width - 0.5;

			movetheta = 100 * (nstartx - ncurrx);

			final double nstarty = 2.0 * starty / height - 0.5;
			final double ncurry = 2.0 * curry / height - 0.5;

			movephi = 100 * (nstarty - ncurry);
		}
		else{
			final float deltax = startx - currx;
			movex = deltax * (maxx - minx) / width;
			final float deltay = starty - curry;
			movey = deltay * (maxy - miny) / height;
		}
	}

	public void next3DMode(){
		selector.next3DMode();
	}

	private Thread plot(final IGraphics graphics, final GraphIterator iterator){
		drawingInfo.setGraphics(graphics);
		drawingInfo.setIterator(iterator);
		drawingInfo.setThread(new Thread(this));
		drawing = true;
		drawingInfo.getThread().start();
		last = drawingInfo.getThread();
		return drawingInfo.getThread();
	}

	public void previous3DMode(){
		selector.previous3DMode();
	}

	private void reinit(final int width, final int height, final GraphList graphList){
		if(width != this.width || height != this.height){
			this.width = width;
			this.height = height;
			setOptimalRangeValues();
		}

		selector.setMode(graphList);

		stopDrawingAndJoin();

		PlotterUtil.setSize(width, height);
		PlotterUtil.setPhi(phi + movephi);
		PlotterUtil.setTheta(theta + movetheta);
	}

	public void resetPosition(){
		minx = -5;
		maxx = 5;
		maxy = 5;
		miny = -5;
		minz = -5;
		maxz = 5;
		setOptimalRangeValues();
	}

	private void rotateClockwise(){
		theta += 15;
		if(theta == 360)
			theta = 0;
	}

	private void rotateCounterClockwise(){
		if(theta == 0)
			theta = 345;
		else
			theta -= 15;
	}

	public void rotateDown(){
		if(invertUD)
			rotateUpwards();
		else
			rotateDownwards();
	}

	private void rotateDownwards(){
		if(phi < 90)
			phi += 15;
	}

	public void rotateLeft(){
		if(invertLR)
			rotateCounterClockwise();
		else
			rotateClockwise();
	}

	public void rotateRight(){
		if(invertLR)
			rotateClockwise();
		else
			rotateCounterClockwise();
	}

	public void rotateUp(){
		if(invertUD)
			rotateDownwards();
		else
			rotateUpwards();
	}

	private void rotateUpwards(){
		if(phi > -90)
			phi -= 15;
	}

	public void run(){
		final IGraphics graphics = drawingInfo.getGraphics();

		if(width * height > 0)
			for(int dt = initialDt; drawing && dt >= minDt && dt > 0; dt >>= 1){
				graphics.setColor(0);
				graphics.fillRect(0, 0, width, height);
				if(selector.isAxisFirstMode(drawingInfo.getIterator().subIterator()))
					AxisPlotter.drawAxis(graphics, this);
				final GraphIterator iterator = drawingInfo.getIterator().subIterator();

				while(drawing && iterator.hasNext()){
					final Graph graph = iterator.next();

					final int color = graph.getColor();
					graphics.setColor(color);
					final Plotter plotter = selector.selectPlotter(graph);
					currentPlotter = plotter;

					lastDrawingWas3D = plotter instanceof ThreePlotter || plotter instanceof ThreeScanner;
					try{
						if(initialDt == MAGIC_DELTA && lastDrawingWas3D)
							plotter.plot(graph, graphics, 4 * dt, this);
						else
							plotter.plot(graph, graphics, dt, this);
					}
					catch(final DrawException e){
						dt = minDt;
						break;
					}
				}
				if(selector.isAxisLaterMode())
					AxisPlotter.drawAxis(graphics, this);

				if(dt == minDt){
					drawEndline(graphics);
					graphics.flushGraphics(true);
				}
				else
					graphics.flushGraphics();
			}

		if(Platform.isJ2ME())
			selector.disposePlotters();

		Log.log(LogLevel.DEBUG, "Drawing done");

		if(!drawing)
			return;

		drawing = false;
		currentPlotter = null;
	}

	public void scale(final double scale){
		Log.log(LogLevel.INFO, "Scaling: " + Double.toString(scale));
		this.scale = 1;
		if(scale < 0){
			Log.log(LogLevel.INFO, "Scaling went negative");
			return;
		}

		final double deltaX = (scale - 1) * (maxx - minx) / 2;
		minx -= deltaX;
		maxx += deltaX;
		Log.log(LogLevel.INFO,
				"Dx:" + Double.toString(deltaX) + " minx:" + Double.toString(minx) + " maxx:" + Double.toString(maxx));

		final double deltaY = (scale - 1) * (maxy - miny) / 2;
		miny -= deltaY;
		maxy += deltaY;
		Log.log(LogLevel.INFO,
				"Dy:" + Double.toString(deltaY) + " miny:" + Double.toString(miny) + " maxy:" + Double.toString(maxy));

		final double deltaZ = (scale - 1) * (maxz - minz) / 2;
		minz -= deltaZ;
		maxz += deltaZ;
	}

	public void set3Ode(final boolean value){
		selector.set3Ode(value);
	}

	public void setCenter(final double centerX, final double centerY){
		final double deltaHalfx = (maxx - minx) / 2.0;
		maxx = centerX + deltaHalfx;
		minx = centerX - deltaHalfx;

		final double deltaHalfy = (maxy - miny) / 2.0;
		maxy = centerY + deltaHalfy;
		miny = centerY - deltaHalfy;
	}

	public void setMaxx(final double nmaxx){
		maxx = nmaxx;
	}

	public void setMaxy(final double nmaxy){
		maxy = nmaxy;
	}

	public void setMaxz(final double nmaxz){
		maxz = nmaxz;
	}

	public void setMinx(final double nminx){
		minx = nminx;
	}

	public void setMiny(final double nminy){
		miny = nminy;
	}

	public void setMinz(final double nminz){
		minz = nminz;
	}

	private void setOptimalRangeValues(){
		if(height * width == 0)
			return;
		final double halfDeltax = (maxx - minx) / 2.0;
		final double halfDeltay = (maxy - miny) / 2.0;
		final double hdmax = halfDeltax > halfDeltay ? halfDeltax : halfDeltay;
		final double centerX = (maxx + minx) / 2.0;
		final double centerY = (maxy + miny) / 2.0;
		if(height > width){
			final double hdmin = hdmax * width / height;
			minx = centerX - hdmin;
			maxx = centerX + hdmin;
			miny = centerY - hdmax;
			maxy = centerY + hdmax;
		}
		else{
			final double hdmin = hdmax * height / width;
			minx = centerX - hdmax;
			maxx = centerX + hdmax;
			miny = centerY - hdmin;
			maxy = centerY + hdmin;
		}
	}

	public void stopDrawing(){
		drawing = false;
	}

	public void stopDrawingAndJoin(){
		drawing = false;

		if(last != null)
			try{
				last.join();
			}
			catch(final InterruptedException e){
			}
	}

	public void syncroPaint(final IGraphics graphics, final GraphList graphList, final int width, final int height,
			final int precision){
		if(precision > 0){
			initialDt = precision;
			minDt = precision;
		}
		else{
			initialDt = -precision;
			minDt = 1;
		}

		if(graphList == null)
			return;

		reinit(width, height, graphList);

		final GraphIterator iterator = graphList.getIterator();
		syncroPlot(graphics, iterator);
	}

	private void syncroPlot(final IGraphics graphics, final GraphIterator iterator){
		drawingInfo.setGraphics(graphics);
		drawingInfo.setIterator(iterator);
		drawing = true;
		run();
	}

	public void tempScale(final double scale){
		if(scale < 0)
			return;
		this.scale = scale;
	}

	public void toggleFreeRoam(){
		freeroam ^= true;
	}

	public void zoomIn(){
		final double qdx = (maxx - minx) / 4.0;
		minx += qdx;
		maxx -= qdx;
		final double qdy = (maxy - miny) / 4.0;
		miny += qdy;
		maxy -= qdy;
		final double qdz = (maxz - minz) / 4.0;
		minz += qdz;
		maxz -= qdz;
	}

	public void zoomOut(){
		final double originalMinx = minx;
		final double originalMaxx = maxx;
		minx = (3.0 * originalMinx - originalMaxx) / 2.0;
		maxx = (3.0 * originalMaxx - originalMinx) / 2.0;
		final double originalMiny = miny;
		final double originalMaxy = maxy;
		miny = (3.0 * originalMiny - originalMaxy) / 2.0;
		maxy = (3.0 * originalMaxy - originalMiny) / 2.0;
		final double originalMinz = minz;
		final double originalMaxz = maxz;
		minz = (3.0 * originalMinz - originalMaxz) / 2.0;
		maxz = (3.0 * originalMaxz - originalMinz) / 2.0;
	}

	public static void setPlotterSelectorFactory(IPlotterSelectorFactory newSelectorFactory){
		selectorFactory = newSelectorFactory;
	}

	public boolean wasLastDrawing3D(){
		return lastDrawingWas3D;
	}
}
