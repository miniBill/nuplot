package meplot.graphics.plotters.three;

import meplot.expressions.Expression;
import meplot.expressions.IValue;
import meplot.expressions.numbers.INumber;
import meplot.graphics.IGraphics;
import meplot.graphics.plotters.ExpressionPlotter;
import meplot.graphics.plotters.IDrawController;
import meplot.graphics.plotters.PlotterUtil;
import meplot.persistence.Settings;
import platform.persistence.Persistence;
import platform.persistence.listeners.BooleanSettingsListener;

public final class ComplexPlotter extends ExpressionPlotter implements BooleanSettingsListener{
	private static final int DOWN = 0x00FF00;
	private static final int ACROSS = 0xFF0000;
	private static final int ORIGINAL = 0x0000FF;
	private int currx;
	private int step;

	public ComplexPlotter(){
		Persistence.registerListener(this);
		drawOriginal = Persistence.loadBoolean(Settings.ORIGINAL);
	}

	public void plot(final Expression expr, final IGraphics graph, final int delta,
			final IDrawController controller){
		step = 10 * delta;
		final int[] rcache = new int[PlotterUtil.getHeight() / step + 1];
		final int[] icache = new int[PlotterUtil.getHeight() / step + 1];

		if(drawOriginal)
			for(currx = 1; controller.isDrawing()
					&& currx < PlotterUtil.getWidth() / step; currx += 4){
				final double projectX = PlotterUtil.projectX(currx * step, controller);
				final IValue subexpr = expr.partialSubstitute('x', projectX);
				for(int y = 1; y <= PlotterUtil.getHeight() / step; y += 4){
					final double projectY = PlotterUtil.projectY(y * step, controller);
					final INumber curr = PlotterUtil.partialC(subexpr, projectY);
					final double currRe = curr.toDouble();
					final int pointx = PlotterUtil.unprojectX(currRe, controller);
					final double currImm = curr.toComplex().immaginary().toDouble();
					final int pointy = PlotterUtil.unprojectY(-currImm, controller);
					graph.setColor(ORIGINAL);
					final int unprojectX = PlotterUtil.unprojectX(projectX, controller);
					final int unprojectY = PlotterUtil.unprojectY(-projectY, controller);
					graph.drawLine(pointx, pointy, unprojectX, unprojectY);
					graph.drawLine(pointx - 4, pointy, pointx + 4, pointy);
					graph.drawLine(pointx - 4, pointy - 4, pointx + 4, pointy + 4);
					graph.drawLine(pointx + 4, pointy - 4, pointx - 4, pointy + 4);
				}
			}

		for(int x = 1; controller.isDrawing() && x < PlotterUtil.getWidth() / step; x++){
			final double projx = PlotterUtil.projectX(x * step, controller);
			final IValue subexpr = expr.partialSubstitute('x', projx);
			for(int y = 1; y <= PlotterUtil.getHeight() / step; y++){
				final double projectY = PlotterUtil.projectY(y * step, controller);
				final INumber curr = PlotterUtil.partialC(subexpr, projectY);

				final double currRe = curr.toDouble();
				final int pointx = PlotterUtil.unprojectX(currRe, controller);
				final double currImm = curr.toComplex().immaginary().toDouble();
				final int pointy = PlotterUtil.unprojectY(-currImm, controller);

				if(y != 1){
					graph.setColor(DOWN);
					graph.drawLine(pointx, pointy, rcache[y - 1], icache[y - 1]);
				}

				if(x != 1){
					graph.setColor(ACROSS);
					graph.drawLine(pointx, pointy, rcache[y], icache[y]);
				}

				rcache[y] = pointx;
				icache[y] = pointy;
			}
		}
	}

	private boolean drawOriginal;

	public void changedSetting(final String name, final boolean arg){
		if(name.equals(Settings.ORIGINAL))
			drawOriginal = arg;
	}

	public int getProgress(){
		return 100 * currx / (PlotterUtil.getWidth() / step);
	}
}
