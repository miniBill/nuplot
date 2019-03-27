package coplot;

import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Vector;

import javax.imageio.ImageIO;

import meplot.graphics.AbstractGraphics;
import meplot.graphics.DrawController;
import meplot.graphics.graphs.GraphList;
import meplot.parser.GraphParser;
import meplot.parser.ParserException;
import platform.log.Log;
import platform.log.SilentLogger;

public final class Program{
	private Program(){
	}

	private static final int[] COLORS = new int[]{0xFF0000, 0x00FF00, 0x0000FF,
			0xFF00FF};

	public static void main(final String[] args){
		Log.setLogger(new SilentLogger());
		final InputStreamReader converter = new InputStreamReader(System.in);
		final BufferedReader in = new BufferedReader(converter);
		final List<String> functions = new Vector<String>();
		String filename = null;
		try{
			if(args.length < 1)
				filename = in.readLine();
			else
				filename = args[0];
			while(true){
				final String test = in.readLine();
				if(test == null || test.length() == 0)
					break;
				functions.add(test);
			}
		}
		catch(final IOException e){
			// ESCA-JAVA0265:
			e.printStackTrace();
			// ESCA-JAVA0267:
			System.err.println("Whathever, giving up :|");
			return;
		}

		final DrawController dc = new DrawController();
		int size = 1000;
		if(args.length > 2)
			size = Integer.parseInt(args[2]);
		if(args.length > 3){
			final int mode = Integer.parseInt(args[3]);
			for(int j = 0; j < mode; j++)
				dc.next3DMode();
		}
		final BufferedImage img = new BufferedImage(size, size,
				BufferedImage.TYPE_INT_ARGB);
		final AbstractGraphics g = new WrapperGraphics(img.getGraphics());
		final GraphList gl = new GraphList();
		int i = 0;
		for(final String test : functions)
			try{
				gl.add(GraphParser.parse(test, COLORS[i++ % COLORS.length]));
			}
			catch(final ParserException e){
				// ESCA-JAVA0267:
				System.err.println(e.getMessage());
				return;
			}
		if(gl.length() == 0)
			return;
		dc.syncroPaint(g, gl, size, size, 1);
		try{
			if(filename.equals("-"))
				ImageIO.write(img, filetype(args), System.out);
			else{
				final File output = new File(filename);
				ImageIO.write(img, filetype(args), output);
			}
		}
		catch(final IOException e1){
			// ESCA-JAVA0265:
			e1.printStackTrace();
		}
	}

	private static String filetype(final String[] args){
		return args.length > 1 ? args[1] : "png";
	}
}
