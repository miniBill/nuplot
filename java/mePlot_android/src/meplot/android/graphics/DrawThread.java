package meplot.android.graphics;

import meplot.graphics.DrawController;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.GraphIterator;
import meplot.graphics.graphs.GraphList;
import meplot.graphics.graphs.NormalGraph;
import android.view.SurfaceHolder;

class DrawThread extends Thread{
	/**
	 * Constant for normal mode: no drag/zoom in progress.
	 */
	public static final int NONE = 0;
	/**
	 * Constant to indicate dragging in progress.
	 */
	public static final int DRAG = 1;
	/**
	 * Costant to indicate zooming in progress.
	 */
	public static final int ZOOM = 2;
	private final SurfaceHolder holder;
	private final int width;
	private final int height;

	DrawThread(final SurfaceHolder holder, final int width, final int height){
		this.holder = holder;
		this.width = width;
		this.height = height;
		setName("DrawThread");
	}

	private volatile boolean running = true;
	private volatile boolean suspended = true;
	private volatile boolean force;
	private GraphList graphList;

	@Override
	public void run(){
		running = true;
		while(running){
			final ViewGraphics graphics = new ViewGraphics(holder);
			synchronized(holder){
				force = false;
				if(getMode() == NONE)
					doDraw(graphics, -DrawController.MAGIC_DELTA);
				else
					doDraw(graphics, DrawController.MAGIC_DELTA);
				graphics.dispose();
			}
			try{
				synchronized(this){
					while(suspended && running && !force)
						wait();
				}
			}
			catch(final InterruptedException e){
			}
		}
	}

	private final DrawController CONTROLLER = new DrawController();

	private void doDraw(final ViewGraphics graphics, final int precision){
		CONTROLLER.syncroPaint(graphics, graphList, width, height, precision);
	}

	public synchronized void stopDrawing(){
		running = false;
		CONTROLLER.stopDrawing();
		notifyAll();
	}

	public DrawController getDrawController(){
		return CONTROLLER;
	}

	public void setGraphList(final GraphList graphList){
		this.graphList = graphList;
	}

	private int mode;

	public synchronized void setMode(final int mode){
		this.mode = mode;
		suspended = false;
		CONTROLLER.stopDrawing();
		notifyAll();
		force = true;
		suspended = mode == NONE;
	}

	public synchronized int getMode(){
		return mode;
	}

	public String getFunctons(){
		return graphList.toString();
	}

	public GraphList getGraphList(){
		return graphList;
	}

	public double[] getData(){
		final double[] toret = new double[6];
		toret[0] = CONTROLLER.getMinx();
		toret[1] = CONTROLLER.getMiny();
		toret[2] = CONTROLLER.getMinz();
		toret[3] = CONTROLLER.getMaxx();
		toret[4] = CONTROLLER.getMaxy();
		toret[5] = CONTROLLER.getMaxz();
		return toret;
	}

	public void setData(final double[] data){
		CONTROLLER.setMinx(data[0]);
		CONTROLLER.setMiny(data[1]);
		CONTROLLER.setMinz(data[2]);
		CONTROLLER.setMaxx(data[3]);
		CONTROLLER.setMaxy(data[4]);
		CONTROLLER.setMaxz(data[5]);
	}

	public double getMinx(){
		return CONTROLLER.getMinx();
	}

	public double getMiny(){
		return CONTROLLER.getMiny();
	}

	public double getMaxx(){
		return CONTROLLER.getMaxx();
	}

	public double getMaxy(){
		return CONTROLLER.getMaxy();
	}

	public NormalGraph[] getNormalExplicitGraphs(){
		int i = 0;

		GraphIterator iterator = graphList.getIterator();
		while(iterator.hasNext()){
			final Graph next = iterator.next();
			if(next instanceof NormalGraph && !next.isImplicit()
					&& !next.is3D())
				i++;
		}

		final NormalGraph[] toret = new NormalGraph[i];

		int j = 0;

		iterator = graphList.getIterator();
		while(iterator.hasNext()){
			final Graph next = iterator.next();
			if(next instanceof NormalGraph && !next.isImplicit()
					&& !next.is3D())
				toret[j++] = (NormalGraph)next;
		}

		return toret;
	}

	public void setCenter(final double cx, final double cy){
		CONTROLLER.setCenter(cx, cy);
	}
}
