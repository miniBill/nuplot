package meplot.graphics.plotters.cache;

import java.lang.ref.WeakReference;

import meplot.graphics.plotters.PlotterUtil;

public final class FloatCache {
	public static final float[][] EMPTY = new float[1][1];
	private WeakReference<float[][]> weak = new WeakReference<float[][]>(EMPTY);

	public float[][] getStrong() {
		final float[][] strong = PlotterUtil.buildFCache(weak);
		weak = new WeakReference<float[][]>(strong);
		return strong;
	}

	public float[][] getStrong(final int width, final int height) {
		final float[][] strong = PlotterUtil.buildFCache(weak, width, height);
		weak = new WeakReference<float[][]>(strong);
		return strong;
	}
}
