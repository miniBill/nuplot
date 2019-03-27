package meplot.daplot.gab.plotter.math;

class Quaternion{
	// variabili
	public float x = 0f;
	public float y = 0f;
	public float z = 0f;
	public float w = 1.0f;

	// ///////////////////////////////////
	// costruttori
	Quaternion(){
	}

	Quaternion(final Quaternion q){
		w = q.w;
		x = q.x;
		y = q.y;
		z = q.z;
	}

	Quaternion(final float w, final float x, final float y, final float z){
		this.w = w;
		this.x = x;
		this.y = y;
		this.z = z;
	}

	// ///////////////////////////////////
	// imposta quaternione in base ai angoli di eulero
	public Vector3D getEulerAngles(){
		float sqX, sqY, sqZ, sqW;
		final Vector3D vec = new Vector3D();

		sqX = x * x;
		sqY = y * y;
		sqZ = z * z;
		sqW = w * w;

		vec.x = (float)(Math.atan2(2.0 * (y * z + x * w), -sqX - sqY + sqZ
				+ sqW) * -10430.3783505);
		vec.y = (float)(Math
				.atan2(2.0 * (x * y + z * w), sqX - sqY - sqZ + sqW) * 10430.3783505);
		vec.z = (float)(Math.asin(-2.0 * (x * z - y * w)) * -10430.3783505);

		return vec;
	}

	public Quaternion setEulerAngles(final Vector3D v){
		setEulerAngles(v.x, v.y, v.z);
		return this;
	}

	private Quaternion setEulerAngles(final float yaw, final float pitch,
			final float roll){
		final float ryaw = (float)Math.toRadians(yaw);
		final float rpitch = (float)Math.toRadians(pitch);
		final float rroll = (float)Math.toRadians(roll);
		final float num9 = rroll * 0.5f;
		final float num6 = (float)Math.sin(num9);
		final float num5 = (float)Math.cos(num9);
		final float num8 = rpitch * 0.5f;
		final float num4 = (float)Math.sin(num8);
		final float num3 = (float)Math.cos(num8);
		final float num7 = ryaw * 0.5f;
		final float num2 = (float)Math.sin(num7);
		final float num = (float)Math.cos(num7);
		x = num * num4 * num5 + num2 * num3 * num6;
		y = num2 * num3 * num5 - num * num4 * num6;
		z = num * num3 * num6 - num2 * num4 * num5;
		w = num * num3 * num5 + num2 * num4 * num6;
		return this;
	}

	// ///////////////////////////////////
	// operazioni con quaternioni
	private void inverse(){
		final double d = w * w + x * x + y * y + z * z;
		w /= d;
		x /= d;
		y /= d;
		z /= d;
	}

	// MOLTIPLICAZIONE
	private void times(final Quaternion other){
		final float xx = w * other.x + x * other.w + y * other.z - z * other.y;
		final float yy = w * other.y - x * other.z + y * other.w + z * other.x;
		final float zz = w * other.z + x * other.y - y * other.x + z * other.w;
		final float ww = w * other.w - x * other.x - y * other.y - z * other.z;
		x = xx;
		y = yy;
		z = zz;
		w = ww;
	}

	// DIVISIONE
	public void divides(final Quaternion other){
		inverse();
		times(other);
	}

	// SOMMA
	public void plus(final Quaternion q){
		w += q.w;
		x += q.x;
		y += q.y;
		z += q.z;
	}

	// SOTRAZIONE
	public void minus(final Quaternion q){
		w += q.w;
		x += q.x;
		y += q.y;
		z += q.z;
	}

	// ///////////////////////////////////

}
