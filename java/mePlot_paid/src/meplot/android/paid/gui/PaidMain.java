package meplot.android.paid.gui;

import meplot.android.R;
import meplot.android.gui.activities.Main;
import meplot.graphics.DrawController;
import meplot.graphics.plotters.IPlotterSelector;
import meplot.graphics.plotters.IPlotterSelectorFactory;

public final class PaidMain extends Main{
	public PaidMain(){
		DrawController.setPlotterSelectorFactory(new IPlotterSelectorFactory(){
			@Override
			public IPlotterSelector getPlotterSelector(){
				return new PaidSelector();
			}
		});
	}

	@Override
	protected void setButtons(){
		setGenericButtons();
		setButton(R.id.main_solve, PaidSolver.class);
		setTitle("mePlot");
	}
}
