package meplot.graphics.plotters;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.INumber;

public final class Filler{
	private Filler(){
	}

	public static void threeScannerFillCache(final Expression expr, final int delta,
			final boolean[][] bcache, final IDrawController controller){
		if(expr.isFullDouble())
			threeScannerFillCacheFull(expr, delta, bcache, controller);
		else
			threeScannerFillCacheNonfull(expr, delta, bcache, controller);
	}

	private static void threeScannerFillCacheFull(final ISubstitutible expr,
			final int delta, final boolean[][] bcache, final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue subexpr = expr.partialSubstitute('x', projx);
			for(int z = 0; controller.isDrawing() && z < width; z += delta)
				bcache[x][z] = subexpr.fdvalue('z', PlotterUtil.projectX(z, controller)) > 0;
		}
	}

	private static void threeScannerFillCacheNonfull(final ISubstitutible expr,
			final int delta, final boolean[][] bcache, final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue subexpr = expr.partialSubstitute('x', projx);
			for(int z = 0; controller.isDrawing() && z < width; z += delta)
				bcache[x][z] = subexpr.value('z', PlotterUtil.projectX(z, controller))
						.real().isPositive();
		}
	}

	/**
	 * Fills caches, for contour plot.
	 *
	 * @param rccache
	 *            real cache
	 * @param iccache
	 *            immaginary cache
	 * @return expr.isFullDouble()
	 */
	public static boolean contourFillCache(final Expression expr, final int delta,
			final float[][] rccache, final float[][] iccache,
			final IDrawController controller){
		final boolean toret = expr.isFullDouble();
		if(toret)
			contourFillCacheFull(expr, delta, rccache, iccache, controller);
		else
			contourFillCacheNonfull(expr, delta, rccache, iccache, controller);
		return toret;
	}

	private static void contourFillCacheFull(final ISubstitutible expr, final int delta,
			final float[][] rccache, final float[][] iccache,
			final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		final int height = PlotterUtil.getHeight();
		double minv = Double.MAX_VALUE;
		double maxv = Double.MIN_VALUE;
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue partialValue = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < height; y += delta){
				final double real = fullPartialF(partialValue, y, controller);
				if(real > maxv)
					maxv = real;
				if(real < minv)
					minv = real;
				rccache[x][y] = (float)real;
				iccache[x][y] = 0;
			}
		}
		PlotterUtil.setMinv(minv);
		PlotterUtil.setMaxv(maxv);
	}

	private static void contourFillCacheNonfull(final ISubstitutible expr,
			final int delta, final float[][] rccache, final float[][] iccache,
			final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		final int height = PlotterUtil.getHeight();
		double minv = Double.MAX_VALUE;
		double maxv = Double.MIN_VALUE;
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue partialValue = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < height; y += delta){
				final double projy = -PlotterUtil.projectY(y, controller);
				final INumber finalValue = complexPartialF(partialValue, projy);
				final double real = finalValue.isReal() ? finalValue.toDouble()
						: finalValue.norm();
				if(real > maxv)
					maxv = real;
				if(real < minv)
					minv = real;
				rccache[x][y] = finalValue.toFloat();
				if(finalValue instanceof Complex)
					iccache[x][y] = ((Complex)finalValue).immaginary().toFloat();
				else
					iccache[x][y] = 0;
			}
		}
		PlotterUtil.setMinv(minv);
		PlotterUtil.setMaxv(maxv);
	}

	private static INumber complexPartialF(final IValue expr, final double yval){
		return expr.matrixDvalue('y', yval).value();
	}

	public static void contourFillCache(final Expression expr, final int delta,
			final float[][] rccache, final float[][] iccache, final boolean[][] bcache,
			final IDrawController controller){
		if(expr.isFullDouble())
			contourFillCacheFull(expr, delta, rccache, iccache, bcache, controller);
		else
			contourFillCacheNonfull(expr, delta, rccache, iccache, bcache, controller);
	}

	private static void contourFillCacheFull(final ISubstitutible expr, final int delta,
			final float[][] rccache, final float[][] iccache, final boolean[][] bcache,
			final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		final int height = PlotterUtil.getHeight();
		double minv = Double.MAX_VALUE;
		double maxv = Double.MIN_VALUE;
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue partialValue = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < height; y += delta){
				final double real = fullPartialF(partialValue, y, controller);
				if(real > maxv)
					maxv = real;
				if(real < minv)
					minv = real;
				rccache[x][y] = (float)real;
				iccache[x][y] = 0;
				bcache[x][y] = real >= 0;
			}
		}
		PlotterUtil.setMinv(minv);
		PlotterUtil.setMaxv(maxv);
	}

	private static double fullPartialF(final IValue partialValue, final int y,
			final IDrawController controller){
		return partialValue.fdvalue('y', -PlotterUtil.projectY(y, controller));
	}

	private static void contourFillCacheNonfull(final ISubstitutible expr,
			final int delta, final float[][] rccache, final float[][] iccache,
			final boolean[][] bcache, final IDrawController controller){
		final int width = PlotterUtil.getWidth();
		final int height = PlotterUtil.getHeight();
		double minv = Double.MAX_VALUE;
		double maxv = Double.MIN_VALUE;
		for(int x = 0; x < width; x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue partialValue = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < height; y += delta){
				final double projy = -PlotterUtil.projectY(y, controller);
				final INumber finalValue = complexPartialF(partialValue, projy);
				final double real = finalValue.isReal() ? finalValue.toDouble()
						: finalValue.norm();
				if(real > maxv)
					maxv = real;
				if(real < minv)
					minv = real;
				rccache[x][y] = finalValue.toFloat();
				if(finalValue instanceof Complex)
					iccache[x][y] = ((Complex)finalValue).immaginary().toFloat();
				else
					iccache[x][y] = 0;
				bcache[x][y] = real >= 0;
			}
		}
		PlotterUtil.setMinv(minv);
		PlotterUtil.setMaxv(maxv);
	}

	public static void fillFieldCache(final ISubstitutible expr, final int delta,
			final float[][] rccache, final float[][] iccache,
			final IDrawController controller){
		double maxv = Double.MIN_VALUE;

		for(int x = 0; x < rccache.length; x++){
			final double projx = PlotterUtil.projectX(x * delta, controller);
			final IValue partialValue = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < rccache[0].length; y++){
				final INumber finalValue = complexPartialF(partialValue,
						-PlotterUtil.projectY(y * delta, controller));
				final double norm = finalValue.norm();
				if(norm > maxv)
					maxv = norm;
				rccache[x][y] = finalValue.toFloat();
				iccache[x][y] = finalValue.isReal() ? 0 : finalValue.toComplex()
						.immaginary().toFloat();
			}
		}

		PlotterUtil.setMaxv(maxv);
	}
}
