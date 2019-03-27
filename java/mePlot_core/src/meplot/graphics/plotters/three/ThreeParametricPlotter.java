package meplot.graphics.plotters.three;

import meplot.expressions.IValue;
import meplot.graphics.DrawException;
import meplot.graphics.IDisposable;
import meplot.graphics.IGraphics;
import meplot.graphics.Point3;
import meplot.graphics.graphs.ParametricGraph;
import meplot.graphics.graphs.ThreeParametricGraph;
import meplot.graphics.plotters.AbstractParametricPlotter;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.PlotterUtil;

public final class ThreeParametricPlotter extends AbstractParametricPlotter implements IDisposable{
	public void plot(final ParametricGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException{
		if(graph instanceof ThreeParametricGraph)
			plot((ThreeParametricGraph)graph, graphics, delta, controller);
	}

	private static final double START = -Math.PI / 2.0;
	private static final double END = Math.PI / 2.0;
	private static final int[][] EMPTY_ZBUFFER = new int[1][1];

	private int[][] zbuffer;
	private int cycle;

	private void plot(final ThreeParametricGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException{
		zbuffer = controller.getZBuffer();
		PlotterUtil.draw3DAxis(graphics, zbuffer);

		final IValue funx = graph.getFx();
		final IValue funy = graph.getFy();
		final IValue funz = graph.getFz();

		if(funx.isFullDouble() && funy.isFullDouble() && funz.isFullDouble())
			fullPlot(graphics, delta, funx, funy, funz, controller);
		else
			noFullPlot(graphics, delta, funx, funy, funz, controller);
	}

	private void fullPlot(final IGraphics graphics, final int delta, final IValue funx, final IValue funy,
			final IValue funz, final IDrawController controller) throws DrawException{
		Point3 curr = new Point3();
		Point3 last = new Point3();
		Point3 swap = null;
		for(cycle = -5; cycle < 6; cycle++)
			for(double t = START; controller.isDrawing() && t < END; t += 0.01 * delta){
				final double currt = t + Math.PI * cycle;
				final double xval = funx.fdvalue('t', currt);
				final double yval = funy.fdvalue('t', currt);
				final double zval = funz.fdvalue('t', currt);

				final double projx = PlotterUtil.projectX(xval, controller);
				final double projy = PlotterUtil.projectY(yval, controller);
				final double projz = PlotterUtil.projectZ(zval, controller);

				PlotterUtil.toRotatedScreenSpace(curr, projx, projy, projz);

				if(cycle != -5 || t != START)
					graphics.drawLine(curr, last, zbuffer);
				swap = last;
				last = curr;
				curr = swap;
			}
	}

	private void noFullPlot(final IGraphics graphics, final int delta, final IValue funx, final IValue funy,
			final IValue funz, final IDrawController controller) throws DrawException{
		Point3 curr = new Point3();
		Point3 last = new Point3();
		Point3 swap = null;
		for(cycle = -5; cycle < 6; cycle++)
			for(double t = START; controller.isDrawing() && t < END; t += 0.01 * delta){
				final double currt = t + Math.PI * cycle;
				final double xval = funx.dvalue('t', currt);
				final double yval = funy.dvalue('t', currt);
				final double zval = funz.dvalue('t', currt);

				final double projx = PlotterUtil.projectX(xval, controller);
				final double projy = PlotterUtil.projectY(yval, controller);
				final double projz = PlotterUtil.projectZ(zval, controller);

				PlotterUtil.toRotatedScreenSpace(curr, projx, projy, projz);

				if(cycle != -5 || t != START)
					graphics.drawLine(curr, last, zbuffer);
				swap = last;
				last = curr;
				curr = swap;
			}
	}

	public void dispose(){
		zbuffer = EMPTY_ZBUFFER;
	}

	public int getProgress(){
		return (cycle + 5) * 10;
	}
}
