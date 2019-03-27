package meplot.graphics.plotters.cache;

import java.lang.ref.WeakReference;

import meplot.graphics.plotters.PlotterUtil;

public final class BooleanCache{
	public static final boolean[][] EMPTY = new boolean[1][1];
	private WeakReference weak = new WeakReference(EMPTY);

	public boolean[][] getStrong(final int width, final int height){
		final boolean[][] strong = PlotterUtil.buildBCache(weak, width, height);
		weak = new WeakReference(strong);
		return strong;
	}

	public boolean[][] getStrong(final int width){
		final boolean[][] strong = PlotterUtil.buildBCache(weak, width, width);
		weak = new WeakReference(strong);
		return strong;
	}
}
