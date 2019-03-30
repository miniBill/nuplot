package meplot.graphics.plotters;

import java.lang.ref.WeakReference;

import meplot.expressions.IValue;
import meplot.expressions.numbers.INumber;
import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.Point3;

public final class PlotterUtil {
	private static final double AXDELTA = 0.05;
	private static final double AXLENGTH = 1.5;
	private static final double AXQUASI = AXLENGTH - AXDELTA;

	private static double cosphi;
	private static double sinphi;
	private static double costheta;
	private static double sintheta;

	private static int height;
	private static int width;

	private static double maxv = Double.MIN_VALUE;
	private static double minv = Double.MAX_VALUE;

	public static boolean[][] buildBCache(final WeakReference<boolean[][]> bcacheRef, final int dim1, final int dim2) {
		final boolean[][] bcache = bcacheRef.get();
		if (bcache == null || bcache.length != dim1 || bcache.length > 0 && bcache[0].length != dim2)
			return new boolean[dim1][dim2];
		return bcache;
	}

	public static float[][] buildFCache(final WeakReference<float[][]> rccacheRef) {
		return buildFCache(rccacheRef, width, height);
	}

	public static float[][] buildFCache(final WeakReference<float[][]> rccacheRef, final int dim1, final int dim2) {
		final float[][] rccache = rccacheRef.get();
		if (rccache == null || rccache.length != dim1 || rccache.length > 0 && rccache[0].length != dim2)
			return new float[dim1][dim2];
		return rccache;
	}

	public static void draw3DAxis(final IGraphics graphics, final int[][] zbuffer) throws DrawException {
		drawXAxis(graphics, zbuffer);
		drawYAxis(graphics, zbuffer);
		drawZAxis(graphics, zbuffer);
	}

	private static void drawXAxis(final IGraphics graphics, final int[][] zbuffer) throws DrawException {
		final Point3 pPoint = new Point3();
		final Point3 pOther = new Point3();
		toRotatedScreenSpace(pPoint, AXLENGTH, 0, 0);
		graphics.setColor(0xFF0000);

		toRotatedScreenSpace(pOther, -AXLENGTH, 0, 0);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXQUASI, AXDELTA, AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXQUASI, -AXDELTA, AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXQUASI, -AXDELTA, -AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXQUASI, AXDELTA, -AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
	}

	private static void drawYAxis(final IGraphics graphics, final int[][] zbuffer) throws DrawException {
		final Point3 pPoint = new Point3();
		final Point3 pOther = new Point3();
		toRotatedScreenSpace(pPoint, 0, AXLENGTH, 0);
		graphics.setColor(0x0000FF);

		toRotatedScreenSpace(pOther, 0, -AXLENGTH, 0);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXDELTA, AXQUASI, AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXDELTA, AXQUASI, -AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, -AXDELTA, AXQUASI, -AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, -AXDELTA, AXQUASI, AXDELTA);
		graphics.drawLine(pOther, pPoint, zbuffer);
	}

	private static void drawZAxis(final IGraphics graphics, final int[][] zbuffer) throws DrawException {
		final Point3 pPoint = new Point3();
		final Point3 pOther = new Point3();
		toRotatedScreenSpace(pPoint, 0, 0, AXLENGTH);
		graphics.setColor(0x00FF00);

		toRotatedScreenSpace(pOther, 0, 0, -AXLENGTH);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXDELTA, AXDELTA, AXQUASI);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, -AXDELTA, AXDELTA, AXQUASI);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, -AXDELTA, -AXDELTA, AXQUASI);
		graphics.drawLine(pOther, pPoint, zbuffer);
		toRotatedScreenSpace(pOther, AXDELTA, -AXDELTA, AXQUASI);
		graphics.drawLine(pOther, pPoint, zbuffer);
	}

	// Returns the value of f at x, in screen coordinates
	public static int functionToScreen(final IValue expr, final int col, final IDrawController controller) {
		final double value = expr.dvalue('x', projectX(col, controller));
		if (Double.isNaN(value))
			return Integer.MAX_VALUE;
		return unprojectY(value, controller);
	}

	// Returns the value of f at x, in screen coordinates
	public static int functionToScreenFull(final IValue expr, final int col, final IDrawController controller) {
		final double value = expr.fdvalue('x', projectX(col, controller));
		if (Double.isNaN(value))
			return Integer.MAX_VALUE;
		return unprojectY(value, controller);
	}

	public static int getHeight() {
		return height;
	}

	public static double getMaxv() {
		return maxv;
	}

	public static double getMinv() {
		return minv;
	}

	public static int getWidth() {
		return width;
	}

	/**
	 * expr.value('y',row).
	 */
	public static INumber partialC(final IValue expr, final double row) {
		return expr.value('y', row);
	}

	public static double partialF(final IValue expr, final double row) {
		return expr.dvalue('y', row);
	}

	public static double partialFFull(final IValue expr, final double row) {
		return expr.fdvalue('y', row);
	}

	private static double project(final double val, final double minf, final double maxf, final int mint,
			final int maxt) {
		return (val - minf) / (maxf - minf) * (maxt - mint) + mint;
	}

	private static double project(final double yval, final int width, final double min, final double max) {
		return yval * (max - min) / width + min;
	}

	public static void project(final Point3 out, final int pointx, final int pointy, final double value) {
		final double projx = project(pointx, width, -1, 1);
		final double projy = project(pointy, height, -1, 1);
		final double projz = 2 * value - 1;

		toRotatedScreenSpace(out, projx, projy, projz);
	}

	public static void project(final Point3 out, final int pointx, final int pointy, final int pointz) {
		final double projx = project(pointx, width, -1, 1);
		final double projy = project(pointy, height, -1, 1);
		final double projz = project(pointz, width, -1, 1);

		toRotatedScreenSpace(out, projx, projy, projz);
	}

	public static double projectX(final double xval, final IDrawController controller) {
		return project(xval, controller.getMinx(), controller.getMaxx(), -1, 1);
	}

	public static double projectX(final int arg, final IDrawController controller) {
		return project(arg, width, controller.getMinx(), controller.getMaxx());
	}

	public static double projectY(final double yval, final IDrawController controller) {
		return project(yval, controller.getMiny(), controller.getMaxy(), -1, 1);
	}

	public static double projectY(final int arg, final IDrawController controller) {
		return project(arg, height, controller.getMiny(), controller.getMaxy());
	}

	public static double projectZ(final double zval, final IDrawController controller) {
		return project(zval, controller.getMinz(), controller.getMaxz(), -1, 1);
	}

	public static void setMaxv(final double maxv) {
		PlotterUtil.maxv = maxv;
	}

	public static void setMinv(final double minv) {
		PlotterUtil.minv = minv;
	}

	public static void setPhi(final double d) {
		double radd = Math.toRadians(d);
		cosphi = Math.cos(radd);
		sinphi = Math.sin(radd);
	}

	public static void setSize(final int width, final int height) {
		PlotterUtil.width = width;
		PlotterUtil.height = height;
	}

	public static void setTheta(final double d) {
		double radd = Math.toRadians(d);
		costheta = Math.cos(radd);
		sintheta = Math.sin(radd);
	}

	public static void toRotatedScreenSpace(final Point3 out, final double pointx, final double pointy,
			final double pointz) {
		/* here we have x,y,z ranging from -1 to 1 */
		final double afirstx = costheta * pointx - sintheta * pointy;
		final double afirsty = sintheta * pointx + costheta * pointy;

		final double asecondy = cosphi * afirsty - sinphi * pointz;
		final double asecondz = cosphi * pointz + sinphi * afirsty;

		final double firstz = 2 - asecondy;
		final double firstx = afirstx / firstz;
		final double firsty = asecondz / firstz;

		final double secondx = (firstx + 1) * width / 2;
		final double secondy = (-firsty + 1) * height / 2;

		out.x = (int) secondx;
		out.y = (int) secondy;
		out.z = (int) (firstz * 10000);
	}

	private static int unproject(final double arg, final int target, final double min, final double max) {
		final double reducedArg = arg - min;
		final double span = max - min;
		final double perc = reducedArg / span;
		return (int) (target * perc);
	}

	public static int unprojectX(final double arg, final IDrawController controller) {
		return unproject(arg, width, controller.getMinx(), controller.getMaxx());
	}

	private PlotterUtil() {

	}

	public static int unprojectY(final double y, final IDrawController controller) {
		return unproject(-y, height, controller.getMiny(), controller.getMaxy());
	}

	public static Point3 project(int pointx, int pointy, int pointz) {
		Point3 toret = new Point3();
		project(toret, pointx, pointy, pointz);
		return toret;
	}
}
