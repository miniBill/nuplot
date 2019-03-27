package meplot.graphics.plotters;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.IValue;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.NormalGraph;
import meplot.persistence.Settings;
import platform.persistence.Persistence;
import platform.persistence.listeners.IntSettingsListener;
import platform.persistence.listeners.StringSettingsListener;

public final class SingleScanner extends NormalPlotter implements IntSettingsListener, StringSettingsListener{
	private static final int BLUEPOS = 0x000001;
	private static final int GREENPOS = 0x000100;
	private static final int REDPOS = 0x010000;
	private static int x;

	public SingleScanner(){
		// ESCA-JAVA0082:
		Persistence.registerListener((IntSettingsListener)this);
		// ESCA-JAVA0082:
		Persistence.registerListener((StringSettingsListener)this);
		final String sderivatives = Persistence.loadString(Settings.DRAWDERIVATIVES, "");
		if(sderivatives.length() == 0)
			drawDerivatives = Persistence.loadInt(Settings.DRAWDERIVATIVES);
		else
			drawDerivatives = Settings.indexof(Settings.DRAWDERIVATIVES, sderivatives);
	}

	public void plot(final NormalGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller){
		final int color = graph.getColor();

		if(drawDerivatives > 0){
			final Expression first = graph.getD();
			if(!first.isZero()){
				final int dcolor = derivativeColor(color);
				graphics.setColor(dcolor);
				doSingleScan(first, graphics, delta, controller);

				if(drawDerivatives == 2){
					final ICalculable second = graph.getDD();
					if(!second.isZero()){
						final int ddcolor = secondDerivativeColor(dcolor);
						graphics.setColor(ddcolor);
						doSingleScan(graph.getDD(), graphics, delta, controller);
					}
				}
			}
		}

		graphics.setColor(color);
		doSingleScan(graph.getExpression(), graphics, delta, controller);
	}

	private static void doSingleScan(final IValue expr, final IGraphics graphics, final int delta,
			final IDrawController controller){
		if(expr.isFullDouble())
			doSingleScanFull(expr, graphics, delta, controller);
		else
			doSingleScanNonfull(expr, graphics, delta, controller);
	}

	private static void doSingleScanFull(final IValue expr, final IGraphics graphics, final int delta,
			final IDrawController controller){
		int last = PlotterUtil.functionToScreenFull(expr, 0, controller);
		for(x = 1; controller.isDrawing() && x < PlotterUtil.getWidth(); x += delta){
			final int curr = PlotterUtil.functionToScreenFull(expr, x, controller);
			if(curr != Integer.MAX_VALUE && last != Integer.MAX_VALUE)
				graphics.drawLine(x - delta, last, x, curr);
			last = curr;
		}
	}

	private static void doSingleScanNonfull(final IValue expr, final IGraphics graphics, final int delta,
			final IDrawController controller){
		int last = PlotterUtil.functionToScreen(expr, 0, controller);
		for(x = 1; controller.isDrawing() && x < PlotterUtil.getWidth(); x += delta){
			final int curr = PlotterUtil.functionToScreen(expr, x, controller);
			if(curr != Integer.MAX_VALUE && last != Integer.MAX_VALUE)
				graphics.drawLine(x - delta, last, x, curr);
			last = curr;
		}
	}

	private static int derivativeColor(final int color){
		final int rcomp = (color & 0xFF0000) >> 16;
		final int gcomp = (color & 0x00FF00) >> 8;
		final int bcomp = (color & 0x0000FF) >> 0;
		final float newr = rcomp * 0.625f;
		final float newg = gcomp * 0.625f;
		final float newb = bcomp * 0.625f;
		final int finalr = (int)Math.floor(newr);
		final int finalg = (int)Math.floor(newg);
		final int finalb = (int)Math.floor(newb);
		return REDPOS * finalr + GREENPOS * finalg + BLUEPOS * finalb;
	}

	private static int secondDerivativeColor(final int color){
		final int newr = (color & 0xFF0000) >> 16;
		final int newg = (color & 0x00FF00) >> 8;
		final int newb = (color & 0x0000FF) >> 0;
		return REDPOS * (newr / 2) + GREENPOS * (newg / 2) + BLUEPOS * (newb / 2);
	}

	private int drawDerivatives;

	public void changedSetting(final String name, final int arg){
		if(name.equals(Settings.DRAWDERIVATIVES))
			drawDerivatives = arg;
	}

	public void changedSetting(final String name, final String arg){
		if(name.equals(Settings.DRAWDERIVATIVES))
			drawDerivatives = Settings.indexof(Settings.DRAWDERIVATIVES, arg);
	}

	public int getProgress(){
		return 100 * x / PlotterUtil.getWidth();
	}
}
