package meplot.graphics;

public abstract class AbstractGraphics implements IGraphics{
	public static final int ALIGN_LEFT = 0;
	public static final int ALIGN_RIGHT = 1;
	public static final int ALIGN_TOP = 0;
	public static final int ALIGN_BOTTOM = 2;

	public abstract void setColor(int color);

	public abstract void fillRect(int left, int top, int width, int height);

	public abstract void drawLine(int xFrom, int yFrom, int xTo, int yTo);

	public final void drawLine(final Point3 from, final Point3 target,
			final int[][] zbuffer) throws DrawException{
		final int fromx = from.x;
		final int fromy = from.y;
		final int fromz = from.z;
		final int tox = target.x;
		final int toy = target.y;
		final int toz = target.z;
		drawLine(fromx, fromy, fromz, tox, toy, toz, zbuffer);
	}

	private void putPixel(final int pixelx, final int pixely, final int pixelz,
			final int[][] zbuffer) throws DrawException{
		if(pixelx >= 0 && pixely >= 0 && pixelx < zbuffer.length
				&& pixely < zbuffer[0].length && pixelz >= 0
				&& zbuffer[pixelx][pixely] >= pixelz){
			zbuffer[pixelx][pixely] = pixelz;
			putPixel(pixelx, pixely);
		}
	}

	// ESCA-JAVA0160:
	protected void putPixel(final int pointx, final int pointy) throws DrawException{
		drawLine(pointx, pointy, pointx, pointy);
	}

	public void drawLine(final int fromx, final int fromy, final int fromz,
			final int tox, final int toy, final int toz, final int[][] zbuffer)
			throws DrawException{
		final int deltax = tox - fromx;
		final int deltay = toy - fromy;
		final int deltaz = toz - fromz;
		float currx = fromx;
		float curry = fromy;
		float currz = fromz;

		final int adeltax = Math.abs(deltax);
		final int adeltay = Math.abs(deltay);
		final float steps = Math.max(adeltax, adeltay);

		final float incX = deltax / steps;
		final float incY = deltay / steps;
		final float incZ = deltaz / steps;

		for(int k = 0; k <= steps; k++){
			final int pointx = (int)(currx + 0.5f);
			final int pointy = (int)(curry + 0.5f);
			final int pointz = (int)(currz + 0.5f);
			putPixel(pointx, pointy, pointz, zbuffer);
			currx += incX;
			curry += incY;
			currz += incZ;
		}
	}

	public void flushGraphics(){
		flushGraphics(false);
	}

	public abstract void flushGraphics(boolean done);

	public final void drawRect(final int left, final int top, final int width,
			final int height){
		drawLine(left, top, left + width, top);
		drawLine(left + width, top, left + width, top + height);
		drawLine(left, top + height, left + width, top + height);
		drawLine(left, top, left, top + height);
	}

	public abstract void drawText(String string, int pointx, int pointy);

	public abstract void setFontSize(float size);

	public abstract void setFontHAlign(int align);

	public abstract void setFontVAlign(int align);
}
