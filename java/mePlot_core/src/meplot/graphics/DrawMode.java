package meplot.graphics;

public final class DrawMode{
	private final int value;

	private DrawMode(final int mode){
		value = mode;
	}

	public static final DrawMode MODE2D = new DrawMode(0);
	public static final DrawMode MODE3D = new DrawMode(1);
	public static final DrawMode MODE3PARAM = new DrawMode(2);
	public static final DrawMode MODE3IMPLICIT = new DrawMode(3);
	public static final DrawMode MODEODE = new DrawMode(4);

	public boolean equals(final Object obj){
		if(this == obj)
			return true;
		if(obj == null)
			return false;
		if(!(obj instanceof DrawMode))
			return false;
		final DrawMode other = (DrawMode)obj;
		return value == other.value;
	}
}
