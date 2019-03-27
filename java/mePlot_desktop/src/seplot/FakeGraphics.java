package seplot;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

import meplot.graphics.AbstractGraphics;

class FakeGraphics extends AbstractGraphics{
	private final Graphics g;

	FakeGraphics(final Graphics g){
		this.g = g;
	}

	public void setColor(final int i){
		g.setColor(new Color(i));
	}

	public void fillRect(final int i, final int i1, final int i2, final int i3){
		g.fillRect(i, i1, i2, i3);
	}

	public void drawLine(final int i, final int i1, final int i2, final int i3){
		g.drawLine(i, i1, i2, i3);
	}

	// ESCA-JAVA0025:
	@Override
	public void flushGraphics(){

	}

	@Override
	public void flushGraphics(final boolean done){

	}

	@Override
	public void drawText(final String string, final int pointx, final int pointy){
		g.drawString(string, pointx, pointy);
	}

	@Override
	public void setFontSize(final float size){
		final Font oldFont = g.getFont();
		final Font font = oldFont.deriveFont(size);
		g.setFont(font);
	}

	@Override
	public void setFontHAlign(final int align){

	}

	@Override
	public void setFontVAlign(final int align){

	}
}
