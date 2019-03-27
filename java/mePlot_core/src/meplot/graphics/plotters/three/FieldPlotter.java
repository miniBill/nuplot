package meplot.graphics.plotters.three;

import meplot.expressions.Expression;
import meplot.graphics.IDisposable;
import meplot.graphics.IGraphics;
import meplot.graphics.plotters.ExpressionPlotter;
import meplot.graphics.plotters.Filler;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.PlotterUtil;
import meplot.graphics.plotters.cache.FloatCache;

public final class FieldPlotter extends ExpressionPlotter implements IDisposable{
	private static final int STEP = 16;
	private final FloatCache rccacheCache = new FloatCache();
	private final FloatCache iccacheCache = new FloatCache();
	private float[][] rccache;
	private float[][] iccache;
	private int x;

	public void plot(final Expression expr, final IGraphics graphics, final int delta,
			final IDrawController controller){
		rccache = rccacheCache.getStrong(PlotterUtil.getWidth() / STEP + 1,
				PlotterUtil.getHeight() / STEP + 1);
		iccache = iccacheCache.getStrong(PlotterUtil.getWidth() / STEP + 1,
				PlotterUtil.getHeight() / STEP + 1);
		Filler.fillFieldCache(expr, delta * STEP, rccache, iccache, controller);

		final double delta2 = delta * STEP / PlotterUtil.getMaxv();
		for(x = 0; x < rccache.length; x += delta)
			for(int y = 0; y < rccache[0].length; y += delta)
				plot(graphics, x, y, rccache[x][y], iccache[x][y], delta2);
	}

	private static void plot(final IGraphics graphics, final int pointx,
			final int pointy, final double rvalue, final double ivalue, final double delta){
		final double douobledeltax = delta * rvalue;
		final double doubledeltay = delta * ivalue;
		final int deltax = (int)(douobledeltax / 2.0);
		final int deltay = (int)(doubledeltay / 2.0);
		final int leftx = pointx * STEP - deltax;
		final int lefty = pointy * STEP + deltay;
		final int rightx = pointx * STEP + deltax;
		final int righty = pointy * STEP - deltay;
		graphics.drawLine(leftx, lefty, rightx, righty);
		graphics.drawLine(rightx - 1, righty, rightx + 1, righty);
		graphics.drawLine(rightx, righty - 1, rightx, righty + 1);
	}

	public void dispose(){
		rccache = FloatCache.EMPTY;
		iccache = FloatCache.EMPTY;
	}

	public int getProgress(){
		return 100 * x / (PlotterUtil.getWidth() / STEP + 1);
	}
}
