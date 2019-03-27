package meplot.daplot.gab.plotter;

import java.util.ArrayList;

import platform.log.Log;

import meplot.daplot.gab.plotter.math.Vector3D;
import meplot.graphics.AbstractGraphics;
import meplot.graphics.DrawController;
import meplot.graphics.graphs.GraphList;
import meplot.parser.GraphParser;
import meplot.parser.ParserException;

class MeplotDraw{
	public static int getColor(final int r, final int g, final int b){
		return (r << 16) + (g << 8) + b;
	}

	private final class DrawOGL extends AbstractGraphics{
		private final ArrayList<StructArray> listLine = new ArrayList<StructArray>();
		private Vector3D color = new Vector3D(0.0f, 0.0f, 0.0f);
		private final OpenGL3D draw;

		private DrawOGL(final OpenGL3D draw){
			this.draw = draw;
		}

		@Override
		public void drawLine(final int xFrom, final int yFrom, final int xTo,
				final int yTo){
			StructArray point1, point2;
			point1 = new StructArray();
			point1.vect3D = new Vector3D(xFrom, yFrom, 0);
			point1.rgb = color;

			point2 = new StructArray();
			point2.vect3D = new Vector3D(xTo, yTo, 0);
			point2.rgb = color;

			listLine.add(point1);
			listLine.add(point2);
		}

		@Override
		public void fillRect(final int left, final int top, final int width,
				final int height){

			drawLine(left, top, left, width);
			drawLine(left, width, width, height);
			drawLine(left, height, left, height);
			drawLine(left, top, top, height);

		}

		@Override
		public void setColor(final int color){
			final int rcomp = (color & 0xFF0000) >> 16;
			final int gcomp = (color & 0x00FF00) >> 8;
			final int bcomp = (color & 0x0000FF) >> 0;
			this.color = new Vector3D(rcomp / 255.0f, gcomp / 255.0f,
					bcomp / 255.0f);
		}

		@Override
		public void flushGraphics(final boolean done){
			// passo la mash
			draw.setMesh(new LineMesh(listLine));
			listLine.clear();
		}

		@Override
		public void drawText(final String string, final int pointx,
				final int pointy){
			// TODO Auto-generated method stub
		}

		@Override
		public void setFontHAlign(final int align){
			// TODO Auto-generated method stub
		}

		@Override
		public void setFontVAlign(final int align){
			// TODO Auto-generated method stub
		}

		@Override
		public void setFontSize(final float f){
			// TODO Auto-generated method stub
		}
	}

	public void genGraph(final OpenGL3D draw, final String[] stringa,
			final int[] colore, final int width, final int height){
		final DrawController dc = new DrawController();
		final DrawOGL graphics = new DrawOGL(draw);
		final GraphList gl = new GraphList();
		// creo la lista di valori
		for(int i = 0; i < stringa.length; i++)
			try{
				gl.add(GraphParser.parse(stringa[i], colore[i]));
			}
			catch(ParserException e){
				Log.log(e);
			}
		dc.doPaint(graphics, gl, width, height);
	}

}
