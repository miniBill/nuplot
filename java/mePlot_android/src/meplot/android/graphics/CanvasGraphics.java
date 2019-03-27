package meplot.android.graphics;

import meplot.graphics.AbstractGraphics;
import meplot.graphics.DrawException;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Paint.Align;

public class CanvasGraphics extends AbstractGraphics{
	private Canvas canvas;
	private final Paint paint = new Paint();

	protected final Canvas getCanvas(){
		return canvas;
	}

	protected final void setCanvas(final Canvas canvas){
		this.canvas = canvas;
	}

	public CanvasGraphics(final Canvas canvas){
		this.canvas = canvas;
	}

	@Override
	public void drawLine(final int xFrom, final int yFrom, final int xTo, final int yTo){
		if(canvas == null)
			return;
		if(xFrom == xTo && yFrom == yTo)
			canvas.drawPoint(xTo, yTo, paint);
		else
			canvas.drawLine(xFrom, yFrom, xTo, yTo, paint);
	}

	@Override
	public void fillRect(final int left, final int top, final int width, final int height){
		if(canvas != null)
			canvas.drawRect(left, top, left + width, top + height, paint);
	}

	@Override
	public void setColor(final int color){
		paint.setColor(0xFF000000 | color);
	}

	@Override
	public void drawText(final String string, final int pointx, final int pointy){
		if(canvas == null)
			return;
		if(valign == AbstractGraphics.ALIGN_TOP){
			final float pointfy = pointy + paint.getTextSize();
			canvas.drawText(string, pointx, pointfy, paint);
		}
		else
			canvas.drawText(string, pointx, pointy, paint);
	}

	@Override
	public void setFontHAlign(final int align){
		if(align == AbstractGraphics.ALIGN_RIGHT)
			paint.setTextAlign(Align.RIGHT);
		else
			paint.setTextAlign(Align.LEFT);
	}

	@Override
	public void setFontSize(final float size){
		paint.setTextSize(size * 3);
	}

	private int valign = AbstractGraphics.ALIGN_TOP;

	@Override
	public void setFontVAlign(final int align){
		valign = align;
	}

	@Override
	protected void putPixel(final int pointx, final int pointy) throws DrawException{
		if(canvas != null)
			canvas.drawPoint(pointx, pointy, paint);
		else
			throw new DrawException();
	}

	@Override
	public void flushGraphics(final boolean done){
		// No need to flush
	}
}
