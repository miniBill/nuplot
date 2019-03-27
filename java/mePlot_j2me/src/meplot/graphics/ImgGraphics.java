package meplot.graphics;

import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;
import javax.microedition.lcdui.game.GameCanvas;

final class ImgGraphics extends MeGraphics{
	private final Graphics inner;
	private final GameCanvas canvas;
	private final Image img;

	ImgGraphics(final Image img, final GameCanvas canvas,
			final Graphics inner){
		super(img.getGraphics(), canvas);
		this.img = img;
		this.inner = inner;
		this.canvas = canvas;
	}

	public void flushGraphics(){
		inner.drawImage(img, 0, 0, 0);
		canvas.flushGraphics();
	}
}
