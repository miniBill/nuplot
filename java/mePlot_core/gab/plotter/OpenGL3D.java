package meplot.daplot.gab.plotter;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.content.Context;
import android.opengl.GLSurfaceView.Renderer;

class OpenGL3D implements Renderer{

	// classe disegno meplot
	private final MeplotDraw meplot = new MeplotDraw();
	// mesh di disegno
	private LineMesh mesh;

	// costruttore
	OpenGL3D(final Context context){
	}

	@Override
	public void onSurfaceCreated(final GL10 gl, final EGLConfig config){
		// //////////////////////////////////////////////////////////
		// Set the background color to black ( rgba ).
		gl.glClearColor(0.0f, 0.0f, 0.0f, 0.5f); // OpenGL docs.
		// Enable Smooth Shading, default not really needed.
		gl.glShadeModel(GL10.GL_SMOOTH); // OpenGL docs.
		// Depth buffer setup.
		gl.glClearDepthf(1.0f); // OpenGL docs.
		// Enables depth testing.
		gl.glEnable(GL10.GL_DEPTH_TEST); // OpenGL docs.
		// The type of depth testing to do.
		gl.glDepthFunc(GL10.GL_LEQUAL); // OpenGL docs.
		// Really nice perspective calculations.
		gl.glOrthof(0, 400, 200, 0, 0, 1);
		/*
		 * gl.glHint(GL10.GL_PERSPECTIVE_CORRECTION_HINT, // OpenGL docs.
		 * GL10.GL_NICEST);
		 */
		// culling
		gl.glEnable(GL10.GL_CULL_FACE);
		gl.glCullFace(GL10.GL_BACK);
		// //////////////////////////////////////////////////////////

		gl.glEnableClientState(GL10.GL_VERTEX_ARRAY);
		gl.glEnableClientState(GL10.GL_COLOR_ARRAY);
		// gl.glEnableClientState(GL10.GL_TEXTURE_COORD_ARRAY);

	}

	@Override
	public void onDrawFrame(final GL10 gl){
		// Setting up the modelview...
		gl.glMatrixMode(GL10.GL_MODELVIEW);
		gl.glLoadIdentity();

		// Clears the screen and depth buffer.
		gl.glClear(GL10.GL_COLOR_BUFFER_BIT | GL10.GL_DEPTH_BUFFER_BIT);
		gl.glLoadIdentity();
		gl.glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

		if(mesh != null)
			mesh.draw(gl);

		// OpenGL finish
		gl.glFlush();
		gl.glFinish();
	}

	// imposta mesh da disegnare
	public void setMesh(final LineMesh mesh){
		this.mesh = mesh;
	}

	@Override
	public void onSurfaceChanged(final GL10 gl, final int width,
			final int height){
		// Sets the current view port to the new size.
		gl.glViewport(0, 0, width, height); // OpenGL docs.
		// Select the projection matrix
		gl.glMatrixMode(GL10.GL_PROJECTION); // OpenGL docs.
		// Reset the projection matrix
		gl.glLoadIdentity(); // OpenGL docs.

		// Calculate the aspect ratio of the window
		gl.glOrthof(0, width, height, 0, 0, 1);
		// generami il grafico
		final String[] app = {"sin(x)", "cos(x)"};
		final int[] color = {MeplotDraw.getColor(0, 255, 0),
				MeplotDraw.getColor(0, 255, 255)};
		meplot.genGraph(this, app, color, width, height);
		/*
		 * GLU.gluPerspective(gl, 45.0f, (float) width / (float) height, 1.0f,
		 * 30.0f);
		 */

		// Select the modelview matrix
		gl.glMatrixMode(GL10.GL_MODELVIEW); // OpenGL docs.
		// Reset the modelview matrix
		gl.glLoadIdentity(); // OpenGL docs.

	}

}
