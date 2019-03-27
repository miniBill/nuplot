package coplot;

import meplot.graphics.AbstractGraphics;

public final class SvgGraphics extends AbstractGraphics{
	private StringBuilder svg = new StringBuilder();
	private String color = "000000";
	private int height;
	private int width;

	@Override
	public void drawLine(final int xa, final int ya, final int xb, final int yb){
		int fb = xb;
		if(xa == xb && ya == yb)
			fb++;
		if(xa < 0 || fb < 0 || ya < 0 || yb < 0 || xa > width || fb > width
				|| ya > height || yb > height)
			return;
		svg.append("<line x1=\"");
		svg.append(xa);
		svg.append("\" y1=\"");
		svg.append(ya);
		svg.append("\" x2=\"");
		svg.append(fb);
		svg.append("\" y2=\"");
		svg.append(yb);
		svg.append("\" style=\"stroke:#");
		svg.append(color);
		svg.append(";stroke-width:1\"/>\n");
	}

	private static String pad(final String hexString){
		switch(hexString.length()){
			case 1:
				return "00000" + hexString;
			case 2:
				return "0000" + hexString;
			case 3:
				return "000" + hexString;
			case 4:
				return "00" + hexString;
			case 5:
				return "0" + hexString;
			default:
				return hexString;
		}
	}

	@Override
	public void fillRect(final int i, final int j, final int width,
			final int height){
		if(i == 0 && j == 0 && width > 60 && height > 60){
			this.width = width;
			this.height = height;
			svg = new StringBuilder(
					"<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"no\"?>\n"
							+ "<!DOCTYPE svg PUBLIC \"-//W3C//Dtd SVG 1.1//EN\" "
							+ "\"http://www.w3.org/Graphics/SVG/1.1/Dtd/svg11.dtd\">\n"
							+ "<svg width=\""
							+ width
							+ "\" height=\""
							+ height
							+ "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n"
							+ "<rect width=\"" + width + "\" height=\""
							+ height + "\" style=\"fill:black\"/>\n");
		}
		else{
			svg.append("<rect width=\"");
			svg.append(width);
			svg.append("\" height=\"");
			svg.append(height);
			svg.append("\" style=\"fill:#");
			svg.append(pad(color));
			svg.append("\"/>\n");
		}
	}

	@Override
	public void setColor(final int i){
		color = pad(Integer.toHexString(i));
	}

	@Override
	public String toString(){
		return svg + "</svg>";
	}

	@Override
	public void drawText(final String string, final int pointx, final int pointy){
	}

	@Override
	public void flushGraphics(final boolean done){
	}

	@Override
	public void setFontHAlign(final int align){
	}

	@Override
	public void setFontSize(final float f){
	}

	@Override
	public void setFontVAlign(final int align){
	}
}
