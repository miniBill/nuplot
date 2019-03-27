package meplot.graphics;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;
import javax.microedition.lcdui.game.GameCanvas;

import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.GraphList;

public final class MeDrawCanvas extends GameCanvas{
	public MeDrawCanvas(){
		super(false);
	}

	private DrawController controller;
	private Image buffer;

	public void paint(final Graphics graphics){
		if(buffer == null)
			reGen();
		if(buffer != null)
			graphics.drawImage(buffer, 0, 0, 0);
	}

	protected void keyPressed(final int keyCode){
		if(DialpadInterface.input(convert(keyCode), controller))
			reGen();
		super.keyPressed(keyCode);
	}

	private static int convert(final int keyCode){
		switch(keyCode){
			case Canvas.KEY_NUM0:
				return 11;
			case Canvas.KEY_NUM1:
				return 1;
			case Canvas.KEY_NUM2:
			case -1:
				return DialpadInterface.UP;
			case Canvas.KEY_NUM3:
				return 3;
			case Canvas.KEY_NUM4:
			case -3:
				return 4;
			case Canvas.KEY_NUM5:
				return 5;
			case Canvas.KEY_NUM6:
			case -4:
				return 6;
			case Canvas.KEY_NUM7:
				return 7;
			case Canvas.KEY_NUM8:
			case -2:
				return 8;
			case Canvas.KEY_NUM9:
				return 9;
				// ESCA-JAVA0040:
			case Canvas.KEY_STAR:
				return 10;
			case Canvas.KEY_POUND:
				return 12;
			case -19: // Back
				return 100;
			default:
				return keyCode * 100;
		}
	}

	private void reGen(){
		final int width = getWidth();
		final int height = getHeight();
		buffer = Image.createImage(width, height);
		touchDrawController();

		if(buffer != null){
			final ImgGraphics graphics = new ImgGraphics(buffer, this,
					getGraphics());
			controller.doPaint(graphics, graphList, width, height);
		}
	}

	private void touchDrawController(){
		if(controller == null)
			controller = new DrawController();
	}

	private GraphList graphList = new GraphList();

	public void clearF(){
		graphList = new GraphList();
	}

	public void addF(final Graph graph){
		buffer = null;
		graphList.add(graph);
	}

	public void stopDrawing(){
		controller.stopDrawingAndJoin();
	}
}
