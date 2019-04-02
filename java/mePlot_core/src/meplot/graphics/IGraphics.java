package meplot.graphics;

public interface IGraphics{
	void setColor(int color);

	void drawLine(Point3 start, Point3 end, int[][] zbuffer);

	void drawLine(final int startx, final int starty, final int startz, final int tox,
			final int toy, final int toz, int[][] zbuffer);

	void drawLine(int startx, int starty, int end, int endy);

	void drawRect(int left, int top, int width, int height);

	void drawText(String string, int left, int top);

	void setFontSize(float size);

	void setFontVAlign(int align);

	void setFontHAlign(int align);

	void fillRect(int left, int top, int width, int height);

	void flushGraphics();
}
