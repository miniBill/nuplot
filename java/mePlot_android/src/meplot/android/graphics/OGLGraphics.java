package meplot.android.graphics;

import meplot.graphics.AbstractGraphics;
import platform.NotImplementedException;

public class OGLGraphics extends AbstractGraphics{
	@Override
	public void drawLine(final int xFrom, final int yFrom, final int xTo, final int yTo){
		throw new NotImplementedException();
	}

	@Override
	public void drawText(final String string, final int pointx, final int pointy){
		throw new NotImplementedException();
	}

	@Override
	public void fillRect(final int left, final int top, final int width, final int height){
		throw new NotImplementedException();
	}

	@Override
	public void flushGraphics(final boolean done){
		throw new NotImplementedException();
	}

	@Override
	public void setColor(final int color){
		throw new NotImplementedException();
	}

	@Override
	public void setFontHAlign(final int align){
		throw new NotImplementedException();
	}

	@Override
	public void setFontSize(final float size){
		throw new NotImplementedException();
	}

	@Override
	public void setFontVAlign(final int align){
		throw new NotImplementedException();
	}
}
