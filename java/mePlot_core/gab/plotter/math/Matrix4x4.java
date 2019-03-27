package meplot.daplot.gab.plotter.math;

import android.opengl.Matrix;

public class Matrix4x4{

	public final float[] matrix;

	// ///////////////////////////////////

	public Matrix4x4(){
		matrix = new float[16];
		identity();
	}

	public Matrix4x4(final Matrix4x4 mx){
		matrix = mx.matrix;
	}

	public Matrix4x4(final float[] mx){
		matrix = mx;
	}

	// ///////////////////////////////////

	public final void identity(){
		matrix[0] = 1;
		matrix[4] = 0;
		matrix[8] = 0;
		matrix[12] = 0;
		matrix[1] = 0;
		matrix[5] = 1;
		matrix[9] = 0;
		matrix[13] = 0;
		matrix[2] = 0;
		matrix[6] = 0;
		matrix[10] = 1;
		matrix[14] = 0;
		matrix[3] = 0;
		matrix[7] = 0;
		matrix[11] = 0;
		matrix[15] = 1;
	}

	public void setMestrix4x4(final float[] pos, final float[] scale,
			final float[] rot){

		Matrix.scaleM(matrix, 0, scale[0], scale[1], scale[2]);
		Matrix.translateM(matrix, 0, pos[0], pos[1], pos[2]);
		Matrix.rotateM(matrix, 0, rot[0], 1, 0, 0);
		Matrix.rotateM(matrix, 0, rot[1], 0, 1, 0);
		Matrix.rotateM(matrix, 0, rot[2], 0, 0, 1);

	}

	public void setMestrix4x4(final float[] pos, final float[] scale,
			final Quaternion rot){

		Matrix.scaleM(matrix, 0, scale[0], scale[1], scale[2]);
		Matrix.translateM(matrix, 0, pos[0], pos[1], pos[2]);
		Matrix.rotateM(matrix, 0, rot.w, rot.x, rot.y, rot.z);

	}

	// ///////////////////////////////////

	public void setMestrix4x4Scale(final float x, final float y, final float z){

		Matrix.scaleM(matrix, 0, x, y, z);

	}

	public void setMestrix4x4Translate(final float x, final float y,
			final float z){

		Matrix.translateM(matrix, 0, x, y, z);

	}

	public void setMestrix4x4Rotation(final float pitch, final float yaw,
			final float roll){

		Matrix.setRotateEulerM(matrix, 0, pitch, yaw, roll);

	}

	public void setMestrix4x4Quaternion(final Quaternion rot){

		Matrix.rotateM(matrix, 0, rot.w, rot.x, rot.y, rot.z);

	}

	// ///////////////////////////////////

	public Vector3D times(final Vector3D v3){
		final float[] r = {0, 0, 0};
		final float[] v = v3.getArray();
		Matrix.multiplyMV(r, 0, matrix, 0, v, 0);
		return new Vector3D(r[0], r[1], r[2]);
	}

	public Matrix4x4 times(final Matrix4x4 mx){
		final float[] r = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1};
		Matrix.multiplyMM(r, 0, matrix, 0, mx.matrix, 0);
		return new Matrix4x4(r);
	}

	// ///////////////////////////////////

}
