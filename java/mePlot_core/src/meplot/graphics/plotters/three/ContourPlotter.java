package meplot.graphics.plotters.three;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Complex;
import meplot.expressions.operations.BooleanOp;
import meplot.graphics.DrawUtils;
import meplot.graphics.IDisposable;
import meplot.graphics.IGraphics;
import meplot.graphics.plotters.ExpressionPlotter;
import meplot.graphics.plotters.Filler;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.PlotterSelector;
import meplot.graphics.plotters.PlotterUtil;
import meplot.graphics.plotters.cache.FloatCache;

public final class ContourPlotter extends ExpressionPlotter implements IDisposable{
	private final FloatCache rccacheCache = new FloatCache();
	private final FloatCache iccacheCache = new FloatCache();
	private float[][] rccache = FloatCache.EMPTY;
	private float[][] iccache = FloatCache.EMPTY;
	private int currx;

	public void plot(final Expression expr, final IGraphics graphics, final int delta, final IDrawController controller){
		rccache = rccacheCache.getStrong();
		iccache = iccacheCache.getStrong();
		final boolean isFull = Filler.contourFillCache(expr, delta, rccache, iccache, controller);
		boolean diseq = expr instanceof BooleanOp && ((BooleanOp)expr).isDisequation();

		if(isFull)
			fullPlot(graphics, delta, controller, diseq);
		else
			nonFullPlot(graphics, delta, controller, diseq);
	}

	public void dispose(){
		rccache = FloatCache.EMPTY;
		iccache = FloatCache.EMPTY;
	}

	private void fullPlot(final IGraphics graphics, final int delta, final IDrawController controller,
			final boolean diseq){
		for(currx = 0; currx < PlotterUtil.getWidth(); currx += delta)
			for(int y = 0; controller.isDrawing() && y < PlotterUtil.getHeight(); y += delta){
				float real = rccache[currx][y];
				if(isOk(real) && (!diseq || real > 0)){
					if(mode == PlotterSelector.GRAYCONTOUR){
						final int floored = (int)Math.floor((real - PlotterUtil.getMinv()) * 0xFF
								/ (PlotterUtil.getMaxv() - PlotterUtil.getMinv()));
						graphics.setColor(0x010101 * floored);
					}
					else
						graphics.setColor(DrawUtils.getColor(real, PlotterUtil.getMinv(), PlotterUtil.getMaxv()));
					if(delta == 1)
						graphics.drawLine(currx, y, currx, y);
					else
						graphics.drawRect(currx + delta / 4, y + delta / 4, delta / 2, delta / 2);
				}
			}

		if(!clean && delta <= 4)
			drawZero(graphics, delta);
	}

	private static boolean isOk(float input){
		return !Double.isNaN(input) && !Double.isInfinite(input);
	}

	private void nonFullPlot(final IGraphics graphics, final int delta, final IDrawController controller,
			final boolean diseq){
		for(currx = 0; currx < PlotterUtil.getWidth(); currx += delta)
			for(int y = 0; controller.isDrawing() && y < PlotterUtil.getHeight(); y += delta){
				float real = rccache[currx][y];
				float immaginary = iccache[currx][y];
				if(isOk(real) && (!diseq || real > 0) && isOk(immaginary)){
					if(mode == PlotterSelector.GRAYCONTOUR){
						final int floored = (int)Math.floor((Complex.norm(real, immaginary) - PlotterUtil.getMinv())
								* 0xFF / (PlotterUtil.getMaxv() - PlotterUtil.getMinv()));
						graphics.setColor(0x010101 * floored);
					}
					else
						graphics.setColor(DrawUtils.getColor(real, immaginary, PlotterUtil.getMinv(),
								PlotterUtil.getMaxv()));
					if(delta == 1)
						graphics.drawLine(currx, y, currx, y);
					else
						graphics.drawRect(currx + delta / 4, y + delta / 4, delta / 2, delta / 2);
				}
			}

		if(!clean && delta <= 4)
			drawZero(graphics, delta);
	}

	private void drawZero(final IGraphics graphics, final int delta){
		graphics.setColor(0x000000);

		for(int y = 0; y < PlotterUtil.getHeight(); y += delta){
			boolean last = rccache[0][y] >= 0;
			for(int x = 0; x < PlotterUtil.getWidth(); x += delta){
				final boolean current = rccache[x][y] >= 0;
				if(last ^ current)
					graphics.drawRect(x + delta / 4, y + delta / 4, delta / 2, delta / 2);
				last = current;
			}
		}

		for(int x = 0; x < PlotterUtil.getWidth(); x += delta){
			boolean last = rccache[x][0] >= 0;
			for(int y = 0; y < PlotterUtil.getHeight(); y += delta){
				final boolean current = rccache[x][y] >= 0;
				if(last ^ current)
					graphics.drawRect(x + delta / 4, y + delta / 4, delta / 2, delta / 2);
				last = current;
			}
		}
	}

	private int mode = PlotterSelector.CONTOUR;
	private boolean clean = true;

	public void setMode(final int mode, final boolean clean){
		this.mode = mode;
		this.clean = clean;
	}

	public int getProgress(){
		return 100 * currx / PlotterUtil.getWidth();
	}
}
