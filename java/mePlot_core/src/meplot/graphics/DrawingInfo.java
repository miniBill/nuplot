package meplot.graphics;

import meplot.graphics.graphs.Graph;
import platform.lists.IIterator;

final class DrawingInfo {
	private IIterator<Graph> iterator;
	private IGraphics graphics;
	private Thread thread;

	public IIterator<Graph> getIterator() {
		return iterator;
	}

	public void setIterator(final IIterator<Graph> iterator) {
		this.iterator = iterator;
	}

	public IGraphics getGraphics() {
		return graphics;
	}

	public void setGraphics(final IGraphics graphics) {
		this.graphics = graphics;
	}

	public Thread getThread() {
		return thread;
	}

	public void setThread(final Thread thread) {
		this.thread = thread;
	}
}
