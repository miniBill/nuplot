package test;

import meplot.graphics.AbstractGraphics;

class StringGraphics extends AbstractGraphics{
	private StringBuffer result = new StringBuffer();

	@Override
	public void drawLine(final int xFrom, final int yFrom, final int xTo,
			final int yTo){
		result.append("L(");
		result.append(xFrom);
		result.append(',');
		result.append(yFrom);
		result.append(',');
		result.append(xTo);
		result.append(',');
		result.append(yTo);
		result.append(')');
	}

	@Override
	public void fillRect(final int left, final int top, final int width,
			final int height){
		result = new StringBuffer();
		setColor(lastcolor);
		result.append("R(");
		result.append(left);
		result.append(',');
		result.append(top);
		result.append(',');
		result.append(width);
		result.append(',');
		result.append(height);
		result.append(')');
	}

	int lastcolor = 0;

	@Override
	public void setColor(final int color){
		result.append("C(");
		result.append(color);
		result.append(')');
		lastcolor = color;
	}

	@Override
	public String toString(){
		return result.toString();
	}

	@Override
	public void flushGraphics(final boolean done){
		result.append("F()");
	}

	@Override
	public void drawText(final String string, final int pointx,
			final int pointy){
		result.append("T(");
		result.append(pointx);
		result.append(',');
		result.append(pointy);
		result.append(',');
		result.append(string);
		result.append(')');
	}

	@Override
	public void setFontHAlign(final int align){
		result.append("H(");
		result.append(align);
		result.append(')');
	}

	@Override
	public void setFontSize(final float f){
		result.append("S(");
		result.append(f);
		result.append(')');
	}

	@Override
	public void setFontVAlign(final int align){
		result.append("V(");
		result.append(align);
		result.append(')');
	}
}