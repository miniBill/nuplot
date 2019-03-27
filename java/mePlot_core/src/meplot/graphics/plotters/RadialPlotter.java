package meplot.graphics.plotters;

import meplot.expressions.Expression;
import meplot.expressions.IValue;
import meplot.expressions.operations.BooleanOp;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.NormalGraph;

public final class RadialPlotter extends NormalPlotter{
	private static int segment;

	public void plot(final NormalGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller){
		if(graph.isRadial())
			plot(graph.getExpression(), graphics, delta, controller);
	}

	private static void plot(final Expression input, final IGraphics graphics,
			final int delta, final IDrawController controller){
		if(input instanceof BooleanOp){
			final IValue expr = ((BooleanOp)input).getRight();
			if(expr.isFullDouble())
				fullPlot(graphics, delta, expr, controller);
			else
				noFullPlot(graphics, delta, expr, controller);
		}
	}

	private static void noFullPlot(final IGraphics graphics, final int delta,
			final IValue expr, final IDrawController controller){
		final double firstr = expr.value('t', 0).toDouble();

		int lasty = PlotterUtil.unprojectY(0, controller);
		int lastx = PlotterUtil.unprojectX(firstr, controller);
		for(segment = 0; segment < 10; segment += 2)
			for(double t = 0; controller.isDrawing() && t < Math.PI * 2.0; t += 0.01 * delta){
				final double radius = expr.value('t', t + Math.PI * segment).real()
						.toDouble();
				final double yval = radius * Math.sin(t);
				final double xval = radius * Math.cos(t);
				final int unprojy = PlotterUtil.unprojectY(yval, controller);
				final int unprojx = PlotterUtil.unprojectX(xval, controller);
				graphics.drawLine(lastx, lasty, unprojx, unprojy);
				lasty = unprojy;
				lastx = unprojx;
			}
	}

	private static void fullPlot(final IGraphics graphics, final int delta,
			final IValue expr, final IDrawController controller){
		final double firstr = expr.value('t', 0).toDouble();

		int lasty = PlotterUtil.unprojectY(0, controller);
		int lastx = PlotterUtil.unprojectX(firstr, controller);
		for(segment = 0; segment < 10; segment += 2)
			for(double t = 0; controller.isDrawing() && t < Math.PI * 2.0; t += 0.01 * delta){
				final double radius = expr.fdvalue('t', t + Math.PI * segment);
				final double yval = radius * Math.sin(t);
				final double xval = radius * Math.cos(t);
				final int unprojy = PlotterUtil.unprojectY(yval, controller);
				final int unprojx = PlotterUtil.unprojectX(xval, controller);
				graphics.drawLine(lastx, lasty, unprojx, unprojy);
				lasty = unprojy;
				lastx = unprojx;
			}
	}

	public int getProgress(){
		return segment * 10;
	}
}
