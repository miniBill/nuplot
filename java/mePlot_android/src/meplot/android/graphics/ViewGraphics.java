package meplot.android.graphics;

import meplot.graphics.IDisposable;
import android.view.SurfaceHolder;

class ViewGraphics extends CanvasGraphics implements IDisposable{
	private final SurfaceHolder holder;

	ViewGraphics(final SurfaceHolder holder){
		super(holder.lockCanvas());
		this.holder = holder;
	}

	public void dispose(){
		if(getCanvas() != null && !isDone)
			holder.unlockCanvasAndPost(getCanvas());
		isDone = false;
	}

	private boolean isDone;

	@Override
	public void flushGraphics(final boolean done){
		if(getCanvas() != null)
			holder.unlockCanvasAndPost(getCanvas());
		isDone = done;
		if(!done)
			setCanvas(holder.lockCanvas());
	}
}
