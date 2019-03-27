package meplot.graphics.plotters;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.operations.BooleanOp;
import meplot.graphics.IGraphics;
import meplot.graphics.graphs.NormalGraph;

public final class XyScanner extends NormalPlotter{
	private static int x;

	private static void drawSquare(final IGraphics graphics, final int delta,
			final int pointx, final int pointy, final int value){
		switch(value){
			case 1:
			case 14:
				graphics.drawLine(pointx - delta, pointy - delta / 2, pointx - delta / 2,
						pointy - delta);
				break;

			case 2:
			case 13:
				graphics.drawLine(pointx - delta / 2, pointy - delta, pointx, pointy
						- delta / 2);
				break;

			case 3:
			case 12:
				graphics.drawLine(pointx - delta, pointy - delta / 2, pointx, pointy
						- delta / 2);
				break;

			case 4:
			case 11:
				graphics.drawLine(pointx - delta / 2, pointy, pointx, pointy - delta / 2);
				break;

			case 5:
				graphics.drawLine(pointx - delta, pointy - delta / 2, pointx - delta / 2,
						pointy);
				graphics.drawLine(pointx - delta / 2, pointy - delta, pointx, pointy
						- delta / 2);
				break;

			case 6:
			case 9:
				graphics.drawLine(pointx - delta / 2, pointy - delta, pointx - delta / 2,
						pointy);
				break;

			case 7:
			case 8:
				graphics.drawLine(pointx - delta, pointy - delta / 2, pointx - delta / 2,
						pointy);
				break;

			case 10:
				graphics.drawLine(pointx - delta, pointy - delta / 2, pointx - delta / 2,
						pointy - delta);
				graphics.drawLine(pointx - delta / 2, pointy, pointx, pointy - delta / 2);
				break;

			case 0:
			case 15:
			default:
				break;
		}
	}

	private static int getValue(final int delta, final boolean[] lastC,
			final boolean[] currC, final int pointy){
		int value = 0;
		if(currC[pointy])
			value |= 4;
		if(currC[pointy - delta])
			value |= 2;
		if(lastC[pointy])
			value |= 8;
		if(lastC[pointy - delta])
			value |= 1;
		return value;
	}

	private static void plotFull(final IGraphics graphics, final int delta,
			final ISubstitutible expr, final IDrawController controller){
		final boolean[] emptyC = new boolean[PlotterUtil.getHeight()];
		final boolean[] lastC = new boolean[PlotterUtil.getHeight()];
		final boolean[] currC = new boolean[PlotterUtil.getHeight()];

		for(x = 0; x < PlotterUtil.getWidth(); x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue subexpr = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < PlotterUtil.getHeight(); y += delta){
				final boolean curr = PlotterUtil.partialFFull(subexpr,
						-PlotterUtil.projectY(y, controller)) <= 0;
				currC[y] = curr;
				if(x == 0 || y == 0)
					continue;
				final int value = getValue(delta, lastC, currC, y);
				drawSquare(graphics, delta, x, y, value);
			}
			System.arraycopy(currC, 0, lastC, 0, currC.length);
			System.arraycopy(emptyC, 0, currC, 0, currC.length);
		}
	}

	private static void plotNonfull(final IGraphics graphics, final int delta,
			final ISubstitutible expr, final IDrawController controller){
		final boolean[] emptyC = new boolean[PlotterUtil.getHeight()];
		final boolean[] lastC = new boolean[PlotterUtil.getHeight()];
		final boolean[] currC = new boolean[PlotterUtil.getHeight()];

		for(x = 0; x < PlotterUtil.getWidth(); x += delta){
			final double projx = PlotterUtil.projectX(x, controller);
			final IValue subexpr = expr.partialSubstitute('x', projx);
			for(int y = 0; controller.isDrawing() && y < PlotterUtil.getHeight(); y += delta){
				final boolean curr = PlotterUtil.partialF(subexpr,
						-PlotterUtil.projectY(y, controller)) <= 0;
				currC[y] = curr;
				if(x == 0 || y == 0)
					continue;
				final int value = getValue(delta, lastC, currC, y);
				drawSquare(graphics, delta, x, y, value);
			}
			System.arraycopy(currC, 0, lastC, 0, currC.length);
			System.arraycopy(emptyC, 0, currC, 0, currC.length);
		}
	}

	public void plot(final NormalGraph graph, final IGraphics graphics, final int delta,
			final IDrawController controller){
		graphics.setColor(graph.getColor());

		Expression expr = graph.getExpression();
		if(expr instanceof BooleanOp){
			final BooleanOp bexpr = (BooleanOp)expr;
			expr = bexpr.getLeft().add(bexpr.getRight().opposite());
		}

		if(expr.isFullDouble())
			plotFull(graphics, delta, expr, controller);
		else
			plotNonfull(graphics, delta, expr, controller);
	}

	public int getProgress(){
		return 100 * x / PlotterUtil.getWidth();
	}
}
