package meplot.graphics.plotters;

import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.operations.OperationsMath;
import meplot.graphics.AbstractGraphics;
import meplot.graphics.IGraphics;
import platform.log.Log;
import platform.log.LogLevel;

public final class AxisPlotter{
	private AxisPlotter(){
	}

	private static final int GRID_COLOR = 0x303040;
	private static final int AXIS_COLOR = 0xFFFFFF;

	private static final double LN2 = ExpMath.ln(2);

	public static void drawAxis(final IGraphics graphics, final IDrawController controller){
		graphics.setColor(GRID_COLOR);

		final double delta = getDelta(controller);

		final double iminx = Math.floor(controller.getMinx() / delta) * delta;
		final double imaxx = Math.ceil(controller.getMaxx() / delta) * delta;
		final double iminy = Math.ceil(-controller.getMaxy() / delta) * delta;
		final double imaxy = Math.floor(-controller.getMiny() / delta) * delta;

		drawGrid(graphics, delta, iminx, imaxx, controller, iminy, imaxy);

		graphics.setColor(AXIS_COLOR);

		final int zerox = getZerox(controller);
		final int zeroy = getZeroy(controller);

		drawLabels(graphics, delta, iminx, imaxx, zerox, zeroy, controller, iminy, imaxy);

		graphics.drawLine(zerox, 0, zerox, PlotterUtil.getHeight());
		graphics.drawLine(0, zeroy, PlotterUtil.getWidth(), zeroy);
	}

	private static int getZerox(final IDrawController controller){
		int zerox = PlotterUtil.unprojectX(0, controller);
		if(zerox >= PlotterUtil.getWidth())
			zerox = PlotterUtil.getWidth() - 1;
		if(zerox < 0)
			zerox = 0;
		return zerox;
	}

	private static int getZeroy(final IDrawController controller){
		int zeroy = PlotterUtil.unprojectY(0, controller);
		if(zeroy >= PlotterUtil.getHeight())
			zeroy = PlotterUtil.getHeight() - 1;
		if(zeroy < 0)
			zeroy = 0;
		return zeroy;
	}

	private static void drawGrid(final IGraphics graphics, final double delta,
			final double iminx, final double imaxx, final IDrawController controller,
			final double miny, final double maxy){
		for(double x = iminx; x <= imaxx; x += delta){
			final int projx = PlotterUtil.unprojectX(x, controller);
			graphics.drawLine(projx, 0, projx, PlotterUtil.getHeight());
		}

		for(double y = miny; y <= maxy; y += delta){
			final int projy = PlotterUtil.unprojectY(y, controller);
			graphics.drawLine(0, projy, PlotterUtil.getWidth(), projy);
		}
	}

	private static double getDelta(final IDrawController controller){
		final double diff = controller.getMaxx() - controller.getMinx();
		final int pow = (int)Math.floor(ExpMath.ln(diff) / LN2) - 6;
		final double delta = OperationsMath.pow(2.0, pow + 3);
		if(delta < 0)
			Log.log(LogLevel.ERROR, "LOLWTFNEGATIVEDELTA");
		return delta;
	}

	private static void drawLabels(final IGraphics graphics, final double delta,
			final double iminx, final double imaxx, final int zerox, final int zeroy,
			final IDrawController controller, final double iminy, final double imaxy){
		graphics.setFontSize(4.0f);

		if(zeroy > PlotterUtil.getHeight() / 2)
			graphics.setFontVAlign(AbstractGraphics.ALIGN_BOTTOM);
		else
			graphics.setFontVAlign(AbstractGraphics.ALIGN_TOP);

		for(double x = iminx; x <= imaxx; x += delta){
			final int projx = PlotterUtil.unprojectX(x, controller);
			graphics.drawText(Double.toString(x), projx, zeroy);
		}

		if(zerox > PlotterUtil.getWidth() / 2)
			graphics.setFontHAlign(AbstractGraphics.ALIGN_RIGHT);
		else
			graphics.setFontHAlign(AbstractGraphics.ALIGN_LEFT);

		for(double y = iminy; y <= imaxy; y += delta){
			final int projy = PlotterUtil.unprojectY(y, controller);
			graphics.drawText(Double.toString(y), zerox, projy);
		}
	}
}
