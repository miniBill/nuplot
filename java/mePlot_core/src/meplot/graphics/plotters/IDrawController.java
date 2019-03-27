package meplot.graphics.plotters;

public interface IDrawController{
	boolean isDrawing();

	int[][] getZBuffer();

	void set3Ode(boolean is3d);

	double getMinx();

	double getMiny();

	double getMaxx();

	double getMaxy();

	double getMinz();

	double getMaxz();
}
