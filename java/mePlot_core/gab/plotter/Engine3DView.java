package meplot.daplot.gab.plotter;

import android.content.Context;
import android.opengl.GLSurfaceView;

public class Engine3DView extends GLSurfaceView{
	public Engine3DView(final Context context){
		super(context);
		setRenderer(new OpenGL3D(context));
	}
}
