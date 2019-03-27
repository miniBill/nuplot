package meplot.graphics.plotters;

import meplot.expressions.IValue;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.ParametricGraph;

public final class ParametricPlotter extends AbstractParametricPlotter{
	private static final double START = -Math.PI / 2.0;
	private static final double END = Math.PI / 2.0;
	private static int segment;

	protected void plot(final ParametricGraph graph, final IGraphics graphics,
			final int delta, final IDrawController controller){
		final IValue funx = graph.getFx();
		final IValue funy = graph.getFy();
		if(funx.isFullDouble() && funy.isFullDouble())
			fullPlot(graphics, delta, funx, funy, controller);
		else
			noFullPlot(graphics, delta, funx, funy, controller);
	}

	private static void fullPlot(final IGraphics graphics, final int delta,
			final IValue funx, final IValue funy, final IDrawController controller){
		final double t0 = START + Math.PI * -5;
		final double x0val = funx.fdvalue('t', t0);
		final double y0val = funy.fdvalue('t', t0);
		int lastx = PlotterUtil.unprojectX(x0val, controller);
		int lasty = PlotterUtil.unprojectY(y0val, controller);
		for(segment = -5; segment < 6; segment++)
			for(double t = START; controller.isDrawing() && t < END; t += 0.01 * delta){
				final double currt = t + Math.PI * segment;
				final double xval = funx.fdvalue('t', currt);
				final double yval = funy.fdvalue('t', currt);
				final int projx = PlotterUtil.unprojectX(xval, controller);
				final int projy = PlotterUtil.unprojectY(yval, controller);
				graphics.drawLine(lastx, lasty, projx, projy);
				lastx = projx;
				lasty = projy;
			}
	}

	private static void noFullPlot(final IGraphics graphics, final int delta,
			final IValue funx, final IValue funy, final IDrawController controller){
		final double t0 = START + Math.PI * -5;
		final double x0val = funx.value('t', t0).toDouble();
		final double y0val = funy.value('t', t0).toDouble();
		int lastx = PlotterUtil.unprojectX(x0val, controller);
		int lasty = PlotterUtil.unprojectY(y0val, controller);
		for(segment = -5; segment < 6; segment++)
			for(double t = START; controller.isDrawing() && t < END; t += 0.01 * delta){
				final double currt = t + Math.PI * segment;
				final double xval = funx.value('t', currt).toDouble();
				final double yval = funy.value('t', currt).toDouble();
				final int projx = PlotterUtil.unprojectX(xval, controller);
				final int projy = PlotterUtil.unprojectY(yval, controller);
				graphics.drawLine(lastx, lasty, projx, projy);
				lastx = projx;
				lasty = projy;
			}
	}

	public int getProgress(){
		return (segment + 5) * 10;
	}
}
