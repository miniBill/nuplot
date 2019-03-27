package meplot.android.graphics;

import meplot.graphics.DialpadInterface;
import meplot.graphics.DrawController;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.Path;
import android.graphics.Rect;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.View;

public class OverlayView extends View{
	private static final int OUTER_RADIUS = 24;
	private static final int INNER_RADIUS = 16;

	public OverlayView(final Context context){
		super(context);
	}

	public OverlayView(final Context context, final AttributeSet attrs){
		super(context, attrs);
	}

	public OverlayView(final Context context, final AttributeSet attrs, final int defStyle){
		super(context, attrs, defStyle);
	}

	private int shown;
	private int intensity;
	private DrawController controller;

	public final void showButton(final int button, final DrawController controller){
		shown = button;
		intensity = 0xFF;
		this.controller = controller;
	}

	public final void decay(){
		intensity -= 0x10;
		if(intensity < 0)
			intensity = 0;
	}

	private final Rect bounds = new Rect();

	@Override
	protected final void onDraw(final Canvas canvas){
		super.onDraw(canvas);
		if(intensity == 0)
			return;

		final int x = getX(shown);
		final int y = getY(shown);

		if(x < 0 || y < 0)
			return;

		canvas.getClipBounds(bounds);
		int width3 = bounds.width() / 3;
		int height3 = bounds.height() / 3;

		final int left = bounds.left + x * width3;
		final int right = left + width3;
		final int top = bounds.top + y * height3;
		final int bottom = top + height3;

		final Path path = getPath(left, right, top, bottom);

		final float diameter = Math.min(right - left, bottom - top) / 2F;
		drawCircle(canvas, path, (left + right) / 2, (top + bottom) / 2, diameter);
	}

	private Path getPath(int left, int right, int top, int bottom){
		switch(shown){
			case DialpadInterface.FREE_ROAM:
				return drawFreeRoam(left, top, right, bottom);
			case DialpadInterface.LEFT:
				return drawLeft(left, top, right, bottom);
			case DialpadInterface.UP:
				return drawUp(left, top, right, bottom);
			case DialpadInterface.ZOOM:
				return drawZoom(left, top, right, bottom);
			case DialpadInterface.DOWN:
				return drawDown(left, top, right, bottom);
			case DialpadInterface.RIGHT:
				return drawRight(left, top, right, bottom);
			case DialpadInterface.DEZOOM:
				return drawDezoom(left, top, right, bottom);
			case DialpadInterface.RESET:
			case DialpadInterface.NEXT_MODE:
			default:
				return null;
		}
	}

	private static int getX(int shown){
		switch(shown){
			case DialpadInterface.FREE_ROAM:
			case DialpadInterface.LEFT:
			case DialpadInterface.RESET:
				return 0;

			case DialpadInterface.UP:
			case DialpadInterface.ZOOM:
			case DialpadInterface.DOWN:
				return 1;

			case DialpadInterface.NEXT_MODE:
			case DialpadInterface.RIGHT:
			case DialpadInterface.DEZOOM:
				return 2;

			default:
				return -1;
		}
	}

	private static int getY(int shown){
		switch(shown){
			case DialpadInterface.FREE_ROAM:
			case DialpadInterface.UP:
			case DialpadInterface.NEXT_MODE:
				return 0;

			case DialpadInterface.LEFT:
			case DialpadInterface.ZOOM:
			case DialpadInterface.RIGHT:
				return 1;

			case DialpadInterface.RESET:
			case DialpadInterface.DOWN:
			case DialpadInterface.DEZOOM:
				return 2;

			default:
				return -1;
		}
	}

	private void drawCircle(final Canvas canvas, final Path path, final int centerX, final int centerY,
			final float diameter){
		final Paint paint = new Paint();
		paint.setColor(0xFFFF00 | intensity << 24);
		paint.setStyle(Style.FILL_AND_STROKE);
		canvas.drawCircle(centerX, centerY, diameter, paint);
		if(path != null){
			paint.setColor(paint.getColor() ^ 0x00FF00);
			canvas.drawPath(path, paint);
		}
	}

	private static Path drawDezoom(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX - 30, centerY - 6);
		path.lineTo(centerX + 30, centerY - 6);
		path.lineTo(centerX + 30, centerY + 6);
		path.lineTo(centerX - 30, centerY + 6);
		return path;
	}

	private static Path drawRight(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX - 20, centerY - 25);
		path.lineTo(centerX + 30, centerY);
		path.lineTo(centerX - 20, centerY + 25);
		return path;
	}

	private static Path drawDown(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX - 25, centerY - 20);
		path.lineTo(centerX, centerY + 30);
		path.lineTo(centerX + 25, centerY - 20);
		return path;
	}

	private static Path drawZoom(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX - 6, centerY - 30);
		path.lineTo(centerX + 6, centerY - 30);
		path.lineTo(centerX + 6, centerY - 6);
		path.lineTo(centerX + 30, centerY - 6);
		path.lineTo(centerX + 30, centerY + 6);
		path.lineTo(centerX + 6, centerY + 6);
		path.lineTo(centerX + 6, centerY + 30);
		path.lineTo(centerX - 6, centerY + 30);
		path.lineTo(centerX - 6, centerY + 6);
		path.lineTo(centerX - 30, centerY + 6);
		path.lineTo(centerX - 30, centerY - 6);
		path.lineTo(centerX - 6, centerY - 6);
		return path;
	}

	private Path drawFreeRoam(final int left, final int top, final int right, final int bottom){
		final Path path = new Path();
		if(controller.isFreeroamMode()){
			final int centerX = (left + right) / 2;
			final int centerY = (top + bottom) / 2;
			if(controller.isFreeroam())
				spin(centerX, centerY, path);
			else
				arrowedCross(centerX, centerY, path);
		}
		return path;
	}

	private static Path drawLeft(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX + 20, centerY - 25);
		path.lineTo(centerX - 30, centerY);
		path.lineTo(centerX + 20, centerY + 25);
		return path;
	}

	private static Path drawUp(final int left, final int top, final int right, final int bottom){
		final int centerX = (left + right) / 2;
		final int centerY = (top + bottom) / 2;
		final Path path = new Path();
		path.setLastPoint(centerX - 25, centerY + 20);
		path.lineTo(centerX, centerY - 30);
		path.lineTo(centerX + 25, centerY + 20);
		return path;
	}

	private static void spin(final int centerX, final int centerY, final Path path){
		path.setLastPoint(centerX, centerY - 32);
		path.lineTo(centerX - 14, centerY - 20);
		path.lineTo(centerX, centerY - 8);
		path.arcTo(new RectF(centerX - INNER_RADIUS, centerY - INNER_RADIUS, centerX + INNER_RADIUS, centerY
				+ INNER_RADIUS), -90, 300);
		path.arcTo(new RectF(centerX - OUTER_RADIUS, centerY - OUTER_RADIUS, centerX + OUTER_RADIUS, centerY
				+ OUTER_RADIUS), 210, -300);
	}

	private static void arrowedCross(final int centerX, final int centerY, final Path path){
		path.setLastPoint(centerX, centerY - 30);
		path.lineTo(centerX + 10, centerY - 20);
		path.lineTo(centerX + 4, centerY - 20);
		path.lineTo(centerX + 4, centerY - 4);
		path.lineTo(centerX + 20, centerY - 4);
		path.lineTo(centerX + 20, centerY - 10);
		path.lineTo(centerX + 30, centerY);
		path.lineTo(centerX + 20, centerY + 10);
		path.lineTo(centerX + 20, centerY + 4);
		path.lineTo(centerX + 4, centerY + 4);
		path.lineTo(centerX + 4, centerY + 20);
		path.lineTo(centerX + 10, centerY + 20);
		path.lineTo(centerX, centerY + 30);
		path.lineTo(centerX - 10, centerY + 20);
		path.lineTo(centerX - 4, centerY + 20);
		path.lineTo(centerX - 4, centerY + 4);
		path.lineTo(centerX - 20, centerY + 4);
		path.lineTo(centerX - 20, centerY + 10);
		path.lineTo(centerX - 30, centerY);
		path.lineTo(centerX - 20, centerY - 10);
		path.lineTo(centerX - 20, centerY - 4);
		path.lineTo(centerX - 4, centerY - 4);
		path.lineTo(centerX - 4, centerY - 20);
		path.lineTo(centerX - 10, centerY - 20);
	}
}
