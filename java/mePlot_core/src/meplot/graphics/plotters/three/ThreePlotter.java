package meplot.graphics.plotters.three;

import java.lang.ref.WeakReference;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Complex;
import meplot.graphics.DrawException;
import meplot.graphics.DrawUtils;
import meplot.graphics.IDisposable;
import meplot.graphics.IGraphics;
import meplot.graphics.Point3;
import meplot.graphics.plotters.ExpressionPlotter;
import meplot.graphics.plotters.Filler;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.PlotterUtil;
import meplot.graphics.plotters.cache.BooleanCache;
import meplot.graphics.plotters.cache.FloatCache;
import meplot.persistence.Settings;
import platform.persistence.Persistence;
import platform.persistence.listeners.BooleanSettingsListener;

public final class ThreePlotter extends ExpressionPlotter implements BooleanSettingsListener, IDisposable {

	private static final int[][] EMPTY_ZBUFFER = new int[1][1];

	public ThreePlotter() {
		Persistence.registerListener(this);
		autoscale = Persistence.loadBoolean(Settings.AUTOSCALE);
	}

	private final FloatCache rccacheCache = new FloatCache();
	private final FloatCache iccacheCache = new FloatCache();
	private final BooleanCache bcacheCache = new BooleanCache();
	private boolean autoscale;
	private float[][] rccache;
	private float[][] iccache;
	private boolean[][] bcache;
	private double minv;
	private double maxv;
	private int y;
	private WeakReference<int[][]> zbufferWeakRef = new WeakReference<int[][]>(EMPTY_ZBUFFER);

	public void plot(final Expression expr, final IGraphics graphics, final int delta, final IDrawController controller)
			throws DrawException {
		rccache = rccacheCache.getStrong();
		iccache = iccacheCache.getStrong();
		bcache = bcacheCache.getStrong(PlotterUtil.getWidth(), PlotterUtil.getHeight());

		Filler.contourFillCache(expr, delta, rccache, iccache, bcache, controller);
		if (autoscale || PlotterUtil.getMinv() == PlotterUtil.getMaxv()) {
			minv = PlotterUtil.getMinv();
			maxv = PlotterUtil.getMaxv();
		} else {
			minv = controller.getMinz();
			maxv = controller.getMaxz();
		}

		final int[][] zbuffer = getZBuffer();
		PlotterUtil.draw3DAxis(graphics, zbuffer);

		final Point3 zpoint = new Point3();
		final Point3 zminusx = new Point3();
		for (int x = delta; controller.isDrawing() && x < rccache.length; x += delta) {
			getPoint3(zpoint, x, 0);
			if (inBound(zpoint)) {
				getPoint3(zminusx, x - delta, 0);
				if (inBound(zminusx)) {
					setColor(graphics, delta, x, delta);
					graphics.drawLine(zpoint, zminusx, zbuffer);
				}
			}
		}

		final Point3 zminusy = new Point3();
		final Point3 minusx = new Point3();
		final Point3 minusy = new Point3();
		final Point3 point = new Point3();
		for (y = delta; y < rccache[0].length; y += delta) {
			getPoint3(zpoint, 0, y);
			if (inBound(zpoint)) {
				getPoint3(zminusy, 0, y - delta);
				if (inBound(zminusy)) {
					setColor(graphics, delta, delta, y);
					graphics.drawLine(zpoint, zminusy, zbuffer);
				}
			}
			for (int x = delta; controller.isDrawing() && x < rccache.length; x += delta) {
				getPoint3(point, x, y);
				if (inBound(point) && point.z < zbuffer[point.x][point.y]) {
					setColor(graphics, delta, x, y);

					getPoint3(minusx, x - delta, y);
					if (inBound(minusx))
						graphics.drawLine(point, minusx, zbuffer);

					getPoint3(minusy, x, y - delta);
					if (inBound(minusy))
						graphics.drawLine(point, minusy, zbuffer);
				}
			}
		}
	}

	private void setColor(final IGraphics graphics, final int delta, final int x, final int y) {
		if (delta == 1) {
			final boolean sign = bcache[x][y];
			final boolean left = sign ^ bcache[x - delta][y];
			final boolean right = sign ^ bcache[x][y - delta];
			if (left || right) {
				graphics.setColor(0);
				return;
			}
		}
		graphics.setColor(DrawUtils.getColor(rccache[x][y], iccache[x][y], minv, maxv));
	}

	public void dispose() {
		rccache = FloatCache.EMPTY;
		iccache = FloatCache.EMPTY;
		bcache = BooleanCache.EMPTY;
	}

	private static boolean inBound(final Point3 arg) {
		return arg.x > 0 && arg.y > 0 && arg.x < PlotterUtil.getWidth() && arg.y < PlotterUtil.getHeight();
	}

	/**
	 * Sets out to the projected point, or to zero on invalid input.
	 * 
	 * @param out
	 * @param pointx
	 * @param pointy
	 */
	private void getPoint3(Point3 out, final int pointx, final int pointy) {
		final float rval = rccache[pointx][pointy];
		final float ival = iccache[pointx][pointy];
		if (Double.isNaN(rval) || Double.isInfinite(rval) || Double.isNaN(ival) || Double.isInfinite(ival)) {
			out.x = out.y = out.z = 0;
		}
		final Complex current = new Complex(rval, ival);
		final double value = current.isReal() ? current.toDouble() : current.norm();

		final double scaledvalue = (value - minv) / (maxv - minv);
		PlotterUtil.project(out, pointx, pointy, scaledvalue);
	}

	private static WeakReference<int[][]> cleanZBufferWeak = new WeakReference<int[][]>(null);

	public int[][] getZBuffer() {
		int[][] toret = zbufferWeakRef.get();

		int[][] clean = cleanZBufferWeak.get();

		final int height = PlotterUtil.getHeight();
		final int width = PlotterUtil.getWidth();
		if (toret == null || toret.length != width || toret.length == 0 || toret[0].length != height) {
			toret = new int[width][height];
			zbufferWeakRef = new WeakReference<int[][]>(toret);
		}

		if (clean == null || clean.length != width || clean.length == 0 || clean[0].length != height) {
			clean = new int[width][height];
			for (int xz = 0; xz < width; xz++)
				for (int yz = 0; yz < height; yz++)
					clean[xz][yz] = Integer.MAX_VALUE;
			cleanZBufferWeak = new WeakReference<int[][]>(clean);
		}

		for (int i = 0; i < width; i++)
			System.arraycopy(clean[i], 0, toret[i], 0, height);

		return toret;
	}

	public void changedSetting(final String name, final boolean arg) {
		if (name.equals(Settings.AUTOSCALE))
			autoscale = arg;
	}

	public int getProgress() {
		return 100 * y / PlotterUtil.getHeight();
	}
}
