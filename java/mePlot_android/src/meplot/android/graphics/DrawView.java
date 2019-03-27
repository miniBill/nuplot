package meplot.android.graphics;

import java.util.Timer;
import java.util.TimerTask;

import meplot.graphics.DialpadInterface;
import meplot.graphics.DrawController;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.GraphList;
import meplot.graphics.graphs.NormalGraph;
import meplot.parser.GraphParser;
import meplot.parser.ParserException;
import platform.log.Log;
import platform.log.LogLevel;
import android.content.Context;
import android.graphics.PointF;
import android.util.AttributeSet;
import android.util.FloatMath;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.view.View.OnTouchListener;

public class DrawView extends SurfaceView implements SurfaceHolder.Callback, OnTouchListener{
	private static final float MIN_DISTANCE = 10f;
	private static final float GRAB_THRESOLD = 20;
	private DrawThread thread;

	public DrawView(final Context context, final AttributeSet attrs){
		super(context, attrs);

		if(isInEditMode())
			return;
		getHolder().addCallback(this);

		regenThread();

		setFocusable(true);
		setClickable(true);
		setOnTouchListener(this);
	}

	private OverlayView overlay;

	public final void setOverlay(final OverlayView view){
		overlay = view;
		final Timer timer = new Timer();
		timer.schedule(new TimerTask(){
			@Override
			public void run(){
				overlay.decay();
				overlay.postInvalidate();
			}
		}, 50, 50);
	}

	@Override
	public final boolean onKeyDown(final int keyCode, final KeyEvent msg){
		if(keyCode == KeyEvent.KEYCODE_BACK){
			thread.stopDrawing();
			return true;
		}
		return false;
	}

	@Override
	public final boolean onKeyUp(final int keyCode, final KeyEvent msg){
		return false;
	}

	@Override
	public final void surfaceCreated(final SurfaceHolder holder){
		threadStopAndStart();
	}

	/**
	 * Stops thread and restarts it
	 */
	private void threadStopAndStart(){
		threadStop();
		thread.start();
	}

	@Override
	public final void surfaceDestroyed(final SurfaceHolder holder){
		threadStop();
	}

	public void surfaceChanged(final SurfaceHolder holder, final int format, final int width, final int height){
		// Nothing to do
	}

	private void threadStop(){
		thread.stopDrawing();
		while(thread.isAlive())
			try{
				thread.join();
			}
			catch(final InterruptedException e){
			}
		regenThread();
	}

	private static final int[] COLORS = new int[] {0xFF0000, 0x00FF00, 0x0000FF, 0xFF00FF};

	private static boolean parseGraphics(final String[] functions, final GraphList graphList){
		if(functions == null)
			return false;
		for(int i = 0; i < functions.length; i++){
			final String function = functions[i].length() == 0 ? "0" : functions[i];
			try{
				final Graph graph = GraphParser.parse(function, 0xFF000000 | COLORS[i % COLORS.length]);
				graphList.add(graph);
			}
			catch(final ParserException e){
				return false;
			}
		}
		return true;
	}

	private void regenThread(){
		final double[] data;
		if(thread != null)
			data = thread.getData();
		else
			data = null;
		thread = new DrawThread(getHolder(), getWidth(), getHeight());
		if(graphics != null)
			thread.setGraphList(graphics);
		if(data != null)
			thread.setData(data);
	}

	private boolean moved;
	private float oldDist;
	private float newDist;
	private final PointF start = new PointF();
	private final PointF mid = new PointF();
	private boolean pressing = true;
	private GraphList graphics;

	@Override
	public final boolean onTouch(final View arg0, final MotionEvent event){
		final int action = event.getAction();

		// ESCA-JAVA0032:
		switch(action & MotionEvent.ACTION_MASK){
			case MotionEvent.ACTION_DOWN:
				onDown(event);
				break;
			case MotionEvent.ACTION_POINTER_DOWN:
				onPointerDown(event);
				break;
			case MotionEvent.ACTION_UP:
			case MotionEvent.ACTION_POINTER_UP:
				onUp(event);
				break;
			case MotionEvent.ACTION_MOVE:
				onMove(event);
				break;
		}
		return true;
	}

	private void onDown(final MotionEvent event){
		final float eventX = event.getX();
		final float eventY = event.getY();
		start.set(eventX, eventY);
		thread.setMode(DrawThread.DRAG);
		moved = false;
		pressing = true;
	}

	private void onUp(final MotionEvent event){
		final float eventX = event.getX();
		final float eventY = event.getY();
		final DrawController controller = thread.getDrawController();
		if(moved)
			controller.move(start.x, eventX, start.y, eventY);
		else
			if(thread.getMode() == DrawThread.ZOOM)
				finishZoom(event, controller);
			else
				if(pressing){
					final int button = convert(eventX, eventY);
					showButton(button);
					DialpadInterface.input(button, controller);
					controller.move(0, 0, 0, 0);
					thread.setMode(DrawThread.NONE);
				}
		thread.setMode(DrawThread.NONE);
	}

	private void onPointerDown(final MotionEvent event){
		oldDist = spacing(event);
		if(oldDist > MIN_DISTANCE){
			midPoint(mid, event);
			thread.setMode(DrawThread.ZOOM);
		}
	}

	private void onMove(final MotionEvent event){
		final DrawController controller = thread.getDrawController();
		final float eventX = event.getX();
		final float eventY = event.getY();

		final boolean movedEnough;
		if(controller.wasLastDrawing3D()){
			final float reducedThresold = GRAB_THRESOLD / 4;
			movedEnough = Math.abs(start.x - eventX) > reducedThresold || Math.abs(start.y - eventY) > reducedThresold;
		}
		else
			movedEnough = Math.abs(start.x - eventX) > GRAB_THRESOLD || Math.abs(start.y - eventY) > GRAB_THRESOLD;
		if(thread.getMode() == DrawThread.DRAG && movedEnough){
			controller.tempMove(start.x, eventX, start.y, eventY);
			moved = true;
			pressing = false;
		}
		else
			if(thread.getMode() == DrawThread.ZOOM){
				final float tryDist = spacing(event);
				if(tryDist > MIN_DISTANCE){
					newDist = tryDist;
					final float scale = newDist / oldDist;
					if(scale > 0 && scale < 100)
						controller.tempScale(1.0 / scale);
					else
						Log.log(LogLevel.WARNING, "WTF scale is " + Float.toString(scale));
				}
				pressing = false;
			}
	}

	private void finishZoom(final MotionEvent event, final DrawController controller){
		final float tryDist = spacing(event);
		if(tryDist > MIN_DISTANCE)
			newDist = tryDist;
		final float scale = newDist / oldDist;
		Log.log(LogLevel.INFO, "newDist: " + Float.toString(newDist) + " oldDist: " + Float.toString(oldDist)
				+ " scale: " + Float.toString(scale));
		controller.scale(1.0 / scale);
	}

	private static void midPoint(final PointF point, final MotionEvent event){
		final float sumX = event.getX(0) + event.getX(1);
		final float sumY = event.getY(0) + event.getY(1);
		point.set(sumX / 2, sumY / 2);
	}

	private static float spacing(final MotionEvent event){
		final float deltaX = event.getX(0) - event.getX(1);
		final float deltaY = event.getY(0) - event.getY(1);
		return FloatMath.sqrt(deltaX * deltaX + deltaY * deltaY);
	}

	private void showButton(final int button){
		if(overlay == null)
			return;
		overlay.showButton(button, thread.getDrawController());
		overlay.postInvalidate();
	}

	private int convert(final float pointX, final float pointY){
		final boolean left = pointX < getWidth() / 3F;
		final boolean right = pointX > 2 * getWidth() / 3F;
		if(pointY < getHeight() / 3.0f){
			if(left)
				return 12;
			if(right)
				return DialpadInterface.NEXT_MODE;
			return DialpadInterface.UP;
		}
		if(pointY < 2.0f * getHeight() / 3.0f){
			if(left)
				return DialpadInterface.LEFT;
			if(right)
				return DialpadInterface.RIGHT;
			return DialpadInterface.ZOOM;
		}
		if(left)
			return 10;
		if(right)
			return DialpadInterface.DEZOOM;
		return DialpadInterface.DOWN;
	}

	public final boolean setFunctions(final String[] functions){
		final GraphList parsedGraphics = new GraphList();
		if(!parseGraphics(functions, parsedGraphics))
			return false;
		graphics = parsedGraphics;
		thread.setGraphList(graphics);
		return true;
	}

	public final void resetZoom(){
		DialpadInterface.input(10, thread.getDrawController());
		threadStopAndStart();
	}

	public final void stopDrawing(){
		thread.stopDrawing();
	}

	public final void resumeDrawing(){
		threadStopAndStart();
	}

	public final void forceRefresh(){
		thread.setMode(DrawThread.NONE);
	}

	public String getFunctions(){
		return thread.getFunctons();
	}

	public GraphList getGraphList(){
		return thread.getGraphList();
	}

	public void setData(final double[] data){
		thread.setData(data);
	}

	public double[] getData(){
		return thread.getData();
	}

	public double getMinx(){
		return thread.getMinx();
	}

	public double getMiny(){
		return thread.getMiny();
	}

	public double getMaxx(){
		return thread.getMaxx();
	}

	public double getMaxy(){
		return thread.getMaxy();
	}

	public double getCy(){
		return (getMiny() + getMaxy()) / 2.0;
	}

	public double getCx(){
		return (getMinx() + getMaxx()) / 2.0;
	}

	public void setCenter(final double cx, final double cy){
		thread.setCenter(cx, cy);
		threadStopAndStart();
	}

	public NormalGraph[] getNormalExplicitGraphs(){
		return thread.getNormalExplicitGraphs();
	}
}
