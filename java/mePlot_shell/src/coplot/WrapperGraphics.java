package coplot;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

import meplot.graphics.AbstractGraphics;

final class WrapperGraphics extends AbstractGraphics{
	private final Graphics buffer;
	private int valign;
	private int halign;

	WrapperGraphics(final Graphics inner){
		buffer = inner;
	}

	@Override
	public void drawLine(final int arg0, final int arg1, final int arg2,
			final int arg3){
		buffer.drawLine(arg0, arg1, arg2, arg3);
	}

	@Override
	public void fillRect(final int arg0, final int arg1, final int arg2,
			final int arg3){
		buffer.fillRect(arg0, arg1, arg2, arg3);
	}

	@Override
	public void setColor(final int rgb){
		buffer.setColor(new Color(rgb));
	}

	@Override
	public void drawText(final String string, final int pointx, final int pointy){
		int fx = pointx;
		int fy = pointy;
		if(valign == AbstractGraphics.ALIGN_TOP)
			fy += buffer.getFontMetrics().getHeight();
		if(halign == AbstractGraphics.ALIGN_RIGHT)
			fx -= buffer.getFontMetrics().stringWidth(string);
		buffer.drawString(string, fx, fy);
	}

	@Override
	public void flushGraphics(final boolean done){
	}

	@Override
	public void setFontHAlign(final int align){
		halign = align;
	}

	@Override
	public void setFontSize(final float size){
		final Font font = buffer.getFont();
		buffer.setFont(font.deriveFont(size));
	}

	@Override
	public void setFontVAlign(final int align){
		valign = align;
	}
}
