package meplot.graphics;

import javax.microedition.lcdui.Font;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.game.GameCanvas;

class MeGraphics extends AbstractGraphics{
	private final Graphics graphics;
	private final GameCanvas canvas;
	private int halign = Graphics.LEFT;
	private int valign = Graphics.TOP;

	MeGraphics(final Graphics graphics, final GameCanvas canvas){
		this.graphics = graphics;
		this.canvas = canvas;
	}

	public void drawLine(final int arg0, final int arg1, final int arg2,
			final int arg3){
		graphics.drawLine(arg0, arg1, arg2, arg3);
	}

	public void fillRect(final int arg0, final int arg1, final int arg2,
			final int arg3){
		graphics.fillRect(arg0, arg1, arg2, arg3);
	}

	public void setColor(final int rgb){
		graphics.setColor(rgb);
	}

	public void flushGraphics(final boolean done){
		canvas.flushGraphics();
	}

	public void drawText(final String string, final int pointx, final int pointy){
		graphics.drawString(string, pointx, pointy, halign | valign);
	}

	public void setFontSize(final float size){
		final Font old = graphics.getFont();
		final int rsize;
		if(size >= Font.SIZE_LARGE)
			rsize = Font.SIZE_LARGE;
		else
			if(size >= Font.SIZE_MEDIUM)
				rsize = Font.SIZE_MEDIUM;
			else
				rsize = Font.SIZE_SMALL;
		final Font setting = Font.getFont(old.getFace(), old.getStyle(), rsize);
		graphics.setFont(setting);
	}

	public void setFontHAlign(final int align){
		if(align == AbstractGraphics.ALIGN_RIGHT)
			halign = Graphics.RIGHT;
		else
			halign = Graphics.LEFT;
	}

	public void setFontVAlign(final int align){
		if(align == AbstractGraphics.ALIGN_BOTTOM)
			valign = Graphics.BOTTOM;
		else
			valign = Graphics.TOP;
	}
}
