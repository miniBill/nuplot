package meplot.graphics.plotters.three;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
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

public final class ThreeScanner extends ExpressionPlotter implements IDisposable{
	private final BooleanCache bcacheCache = new BooleanCache();
	private boolean[][] bcache = BooleanCache.EMPTY;

	private IGraphics graphics;

	private int[][] zbuffer;
	private int y;

	public void plot(final Expression expr, final IGraphics graphics2, final int delta,
			final IDrawController controller) throws DrawException{
		final int idelta = delta * 4;
		bcache = bcacheCache.getStrong(PlotterUtil.getWidth());

		graphics = graphics2;

		zbuffer = controller.getZBuffer();

		PlotterUtil.draw3DAxis(graphics2, zbuffer);

		Filler.threeScannerFillCache(expr.partialSubstitute('y', controller.getMiny()),
				idelta, bcache, controller);

		if(expr.isFullDouble())
			plotFull(expr, graphics2, delta, controller);
		else
			plotNonfull(expr, graphics2, idelta, controller);
	}

	private void plotFull(final ISubstitutible expr, final IGraphics graphics2,
			final int delta, final IDrawController controller) throws DrawException{
		final int height = PlotterUtil.getHeight();
		final int width = PlotterUtil.getWidth();
		final boolean[] lastline = new boolean[width];

		for(y = delta; y < height; y += delta){
			final double projy = PlotterUtil.projectY(y, controller);
			final ISubstitutible yexpr = expr.partialSubstitute('y', projy);
			IValue xexpr = yexpr.partialSubstitute('x', controller.getMinx());
			for(int z = 0; z < width; z += delta)
				lastline[z] = xexpr.fdvalue('z', PlotterUtil.projectX(z, controller)) > 0;
			for(int x = delta; x < width; x += delta){
				xexpr = yexpr.partialSubstitute('x', PlotterUtil.projectX(x, controller));
				boolean last = xexpr.fdvalue('z', controller.getMinz()) > 0;
				for(int z = delta; controller.isDrawing() && z < width; z += delta){
					final int color = DrawUtils.getColor(z, 0, 0, width);
					graphics2.setColor(color);
					final double projected = PlotterUtil.projectX(z, controller);
					final boolean curr = xexpr.fdvalue('z', projected) > 0;

					// INNER LOOP. LO AND BEHOLD.
					drawTethra(delta, x, y, z, curr, last, lastline);

					lastline[z - delta] = last;
					last = curr;
				}
				lastline[width - delta] = last;
				for(int z = 0; z < width; z += delta)
					bcache[x - delta][z] = lastline[z];
			}
			for(int z = 0; z < width; z += delta)
				bcache[width - delta][z] = lastline[z];
		}
	}

	private void plotNonfull(final ISubstitutible expr, final IGraphics graphics2,
			final int delta, final IDrawController controller) throws DrawException{
		final int width = PlotterUtil.getWidth();
		final boolean[] lastline = new boolean[width];
		for(y = delta; controller.isDrawing() && y < PlotterUtil.getHeight(); y += delta){
			final double projy = PlotterUtil.projectY(y, controller);
			final ISubstitutible yexpr = expr.partialSubstitute('y', projy);
			IValue xexpr = yexpr.partialSubstitute('x', controller.getMinx());
			for(int z = 0; z < width; z += delta){
				final double projected = PlotterUtil.projectX(z, controller);
				lastline[z] = xexpr.value('z', projected).real().isPositive();
			}
			for(int x = delta; x < width; x += delta){
				xexpr = yexpr.partialSubstitute('x', PlotterUtil.projectX(x, controller));
				boolean last = xexpr.value('z', controller.getMinz()).real().isPositive();
				for(int z = delta; z < width; z += delta){
					final int color = DrawUtils.getColor(z, 0, 0, width);
					graphics2.setColor(color);
					final double projected = PlotterUtil.projectX(z, controller);
					final boolean curr = xexpr.value('z', projected).real().isPositive();

					// INNER LOOP. LO AND BEHOLD.
					drawTethra(delta, x, y, z, curr, last, lastline);

					lastline[z - delta] = last;
					last = curr;
				}
				lastline[width - delta] = last;
				for(int z = 0; z < width; z += delta)
					bcache[x - delta][z] = lastline[z];
			}
			for(int z = 0; z < width; z += delta)
				bcache[width - delta][z] = lastline[z];
		}
	}

	private void drawTethra(final int delta, final int pointx, final int pointy,
			final int pointz, final boolean curr, final boolean last,
			final boolean[] lastline) throws DrawException{
		final Point3 base = PlotterUtil.project(pointx, pointy, pointz);
		final Point3 minusx = PlotterUtil.project(pointx - delta, pointy, pointz);
		final Point3 minusy = PlotterUtil.project(pointx, pointy - delta, pointz);
		final Point3 minusz = PlotterUtil.project(pointx, pointy, pointz - delta);
		final Point3 minusyz = PlotterUtil
				.project(pointx, pointy - delta, pointz - delta);
		final Point3 minusxz = PlotterUtil
				.project(pointx - delta, pointy, pointz - delta);
		final Point3 minusxy = PlotterUtil
				.project(pointx - delta, pointy - delta, pointz);
		final Point3 minusall = PlotterUtil.project(pointx - delta, pointy - delta,
				pointz - delta);

		final boolean minusx2 = lastline[pointz];
		final boolean minusxz2 = lastline[pointz - delta];
		final boolean minusy2 = bcache[pointx][pointz];
		final boolean minusyz2 = bcache[pointx - delta][pointz - delta];
		final boolean minusxy2 = bcache[pointx - delta][pointz];

		tethraedon(minusx, minusx2, base, curr, minusz, last, minusxy, minusxy2);
		tethraedon(minusx, minusx2, minusxz, minusxz2, minusz, last, minusxy, minusxy2);
		tethraedon(minusall, minusyz2, minusxz, minusxz2, minusz, last, minusxy, minusxy2);
		tethraedon(minusall, minusyz2, minusyz, minusyz2, minusz, last, minusxy, minusxy2);
		tethraedon(minusy, minusy2, base, curr, minusz, last, minusxy, minusxy2);
		tethraedon(minusy, minusy2, minusyz, minusyz2, minusz, last, minusxy, minusxy2);
	}

	private void tethraedon(final Point3 arg01, final boolean arg02, final Point3 arg11,
			final boolean arg12, final Point3 arg21, final boolean arg22,
			final Point3 arg31, final boolean arg32) throws DrawException{
		final boolean l01 = arg02 ^ arg12;
		final boolean l02 = arg02 ^ arg22;
		final boolean l03 = arg02 ^ arg32;
		final boolean l12 = arg12 ^ arg22;
		final boolean l13 = arg12 ^ arg32;
		final boolean l23 = arg22 ^ arg32;
		if(l01 && l02)
			tethraline(arg01, arg11, arg01, arg21);
		if(l01 && l03)
			tethraline(arg01, arg11, arg01, arg31);
		if(l01 && l12)
			tethraline(arg01, arg11, arg11, arg21);
		if(l01 && l13)
			tethraline(arg01, arg11, arg11, arg31);
		if(l02 && l03)
			tethraline(arg01, arg31, arg01, arg21);
		if(l02 && l12)
			tethraline(arg01, arg21, arg11, arg21);
		if(l02 && l23)
			tethraline(arg01, arg21, arg21, arg31);
		if(l03 && l13)
			tethraline(arg01, arg31, arg11, arg31);
		if(l03 && l23)
			tethraline(arg01, arg31, arg21, arg31);
		if(l12 && l13)
			tethraline(arg11, arg21, arg11, arg31);
		if(l12 && l23)
			tethraline(arg11, arg21, arg21, arg31);
		if(l13 && l23)
			tethraline(arg11, arg31, arg21, arg31);
	}

	private void tethraline(final Point3 arg0, final Point3 arg1, final Point3 arg2,
			final Point3 arg3) throws DrawException{
		final int fromx = (arg0.x + arg1.x) / 2;
		final int fromy = (arg0.y + arg1.y) / 2;
		final int fromz = (arg0.z + arg1.z) / 2;
		final int tox = (arg2.x + arg3.x) / 2;
		final int toy = (arg2.y + arg3.y) / 2;
		final int toz = (arg2.z + arg3.z) / 2;
		graphics.drawLine(fromx, fromy, fromz, tox, toy, toz, zbuffer);
	}

	private static final int[][] EMPTY_ZBUFFER = new int[1][1];

	public void dispose(){
		bcache = BooleanCache.EMPTY;
		zbuffer = EMPTY_ZBUFFER;
	}

	public int getProgress(){
		return 100 * y / PlotterUtil.getHeight();
	}
}
