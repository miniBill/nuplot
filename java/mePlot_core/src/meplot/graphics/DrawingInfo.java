package meplot.graphics;

import meplot.graphics.graphs.GraphIterator;

final class DrawingInfo{
	private GraphIterator iterator;
	private IGraphics graphics;
	private Thread thread;

	public GraphIterator getIterator(){
		return iterator;
	}

	public void setIterator(final GraphIterator iterator){
		this.iterator = iterator;
	}

	public IGraphics getGraphics(){
		return graphics;
	}

	public void setGraphics(final IGraphics graphics){
		this.graphics = graphics;
	}

	public Thread getThread(){
		return thread;
	}

	public void setThread(final Thread thread){
		this.thread = thread;
	}
}
