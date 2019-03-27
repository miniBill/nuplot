package meplot.daplot.gab.plotter.math;

public class Vector3D{
	// variabili
	public float x, y, z;

	// costruttori
	public Vector3D(){
		x = 0;
		y = 0;
		z = 0;
	}

	public Vector3D(final float x, final float y, final float z){
		this.x = x;
		this.y = y;
		this.z = z;
	}

	// copia il vertice in una array
	public float[] getArray(){
		return new float[]{x, y, z};
	}
}
