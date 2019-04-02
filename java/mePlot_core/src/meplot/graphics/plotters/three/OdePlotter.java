package meplot.graphics.plotters.three;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.IComplex;
import meplot.graphics.DrawException;
import meplot.graphics.IGraphics;
import meplot.graphics.Point3;
import meplot.graphics.graphs.NormalGraph;
import meplot.graphics.graphs.OdeGraph;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.NormalPlotter;
import meplot.graphics.plotters.PlotterUtil;
import meplot.numerical.RungeKutta;

public final class OdePlotter extends NormalPlotter{
	private int progress;

	public void plot(final NormalGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller) throws DrawException{
		if(graph instanceof OdeGraph)
			plot((OdeGraph)graph, graphics, delta, controller);
	}

	private void plot(final OdeGraph graph, final IGraphics graphics, final int delta, final IDrawController controller)
			throws DrawException{
		final RungeKutta rungeKutta = graph.getRK();
		if(rungeKutta == null)
			return;

		rungeKutta.start(delta / 10.0);
		if(graph.is3D())
			plot3d(graphics, rungeKutta, controller);
		else
			if(graph.is2D())
				plot2d(graphics, rungeKutta, controller);
			else
				plot1d(graphics, rungeKutta, controller);

		rungeKutta.start(-delta / 10.0);
		if(graph.is3D())
			plot3d(graphics, rungeKutta, controller);
		else
			if(graph.is2D())
				plot2d(graphics, rungeKutta, controller);
			else
				plot1d(graphics, rungeKutta, controller);
	}

	private void plot1d(final IGraphics graphics, final RungeKutta rungeKutta, final IDrawController controller){
		controller.set3Ode(false);

		int lastx = 0;
		int lasty = 0;
		for(progress = 0; controller.isDrawing() && progress < 100; progress++){
			final double yval = rungeKutta.next().dvalue();
			final double xval = rungeKutta.getT();
			final int projx = PlotterUtil.unprojectX(xval, controller);
			final int projy = PlotterUtil.unprojectY(yval, controller);
			if(progress != 0)
				graphics.drawLine(lastx, lasty, projx, projy);
			lastx = projx;
			lasty = projy;
		}
	}

	private void plot2d(final IGraphics graphics, final RungeKutta rungeKutta, final IDrawController controller){
		controller.set3Ode(false);

		int lastx = 0;
		int lasty = 0;
		for(progress = 0; controller.isDrawing() && progress < 100; progress++){
			final IComplex val = rungeKutta.next().value().toComplex();
			final double xval = val.toDouble();
			final double yval = val.immaginary().toDouble();
			final int projx = PlotterUtil.unprojectX(xval, controller);
			final int projy = PlotterUtil.unprojectY(yval, controller);
			if(progress != 0)
				graphics.drawLine(lastx, lasty, projx, projy);
			lastx = projx;
			lasty = projy;
		}
	}

	private void plot3d(final IGraphics graphics, final RungeKutta rungeKutta, final IDrawController controller) {
		final int[][] zbuffer = controller.getZBuffer();
		PlotterUtil.draw3DAxis(graphics, zbuffer);
		controller.set3Ode(true);

		Point3 curr = new Point3();
		Point3 last = new Point3();
		Point3 swap = null;
		for(progress = 0; controller.isDrawing() && progress < 100; progress++){
			final Expression val = rungeKutta.next();
			if(val instanceof Matrix){
				final Matrix mat = (Matrix)val;
				double xval = 0;
				double yval = 0;
				double zval = 0;
				if(mat.getRows() == 3){
					xval = mat.get(0, 0).dvalue();
					yval = mat.get(1, 0).dvalue();
					zval = mat.get(2, 0).dvalue();
				}
				else
					if(mat.getCols() == 3){
						xval = mat.get(0, 0).dvalue();
						yval = mat.get(0, 1).dvalue();
						zval = mat.get(0, 2).dvalue();
					}

				final double projx = PlotterUtil.projectX(xval, controller);
				final double projy = PlotterUtil.projectY(yval, controller);
				final double projz = PlotterUtil.projectZ(zval, controller);

				PlotterUtil.toRotatedScreenSpace(curr, projx, projy, projz);

				if(progress != 0)
					graphics.drawLine(curr, last, zbuffer);
				swap = last;
				last = curr;
				curr = swap;
			}
		}
	}

	public int getProgress(){
		return progress;
	}
}
