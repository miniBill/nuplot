package meplot.daplot.gab.plotter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;

import javax.microedition.khronos.opengles.GL10;


final class LineMesh{
	private final FloatBuffer vertexBuffer;
	private final ShortBuffer indexBuffer;
	private final FloatBuffer colorBuffer;
	private final int nrOfVertices;

	LineMesh(final ArrayList<StructArray> listLine){
		final short[] indices = new short[listLine.size()];
		final float[] vertices = new float[listLine.size() * 3];
		final float[] colors = new float[listLine.size() * 4];
		final int nrOfVertices = listLine.size(); // array3D.length;
		for(int i = 0; i < nrOfVertices; i++){
			vertices[i * 3] = listLine.get(i).vect3D.x;
			vertices[i * 3 + 1] = listLine.get(i).vect3D.y;
			vertices[i * 3 + 2] = listLine.get(i).vect3D.z;

			colors[i * 4] = listLine.get(i).rgb.x;
			colors[i * 4 + 1] = listLine.get(i).rgb.y;
			colors[i * 4 + 2] = listLine.get(i).rgb.z;
			colors[i * 4 + 3] = 1.0f;

			indices[i] = (short)i;
		}

		// short ha 2 bytes, indices * 2 bytes
		final ByteBuffer ibb = ByteBuffer
				.allocateDirect(indices.length * 2);
		ibb.order(ByteOrder.nativeOrder());
		indexBuffer = ibb.asShortBuffer();
		this.nrOfVertices = indices.length;

		// creo il buffer dei vertici
		// float ha 4 bytes, vertices * 4 bytes
		final ByteBuffer vbb = ByteBuffer
				.allocateDirect(vertices.length * 4);
		vbb.order(ByteOrder.nativeOrder());
		vertexBuffer = vbb.asFloatBuffer();

		// creo il buffer dei color
		// float ha 4 bytes, vertices * 4 bytes
		final ByteBuffer cbb = ByteBuffer.allocateDirect(colors.length * 4);
		cbb.order(ByteOrder.nativeOrder());
		colorBuffer = cbb.asFloatBuffer();

		// inserisco le array nei buffers
		indexBuffer.put(indices);
		vertexBuffer.put(vertices);
		colorBuffer.put(colors);

		indexBuffer.position(0);
		vertexBuffer.position(0);
		colorBuffer.position(0);
	}

	public void draw(final GL10 gl){
		gl.glLoadIdentity();
		gl.glVertexPointer(3, GL10.GL_FLOAT, 0, vertexBuffer);
		gl.glColorPointer(4, GL10.GL_FLOAT, 0, colorBuffer);
		gl.glScalef(1, 1, 1);
		gl.glTranslatef(0, 0, 0);
		// gl.glColor4f(0.0f, 0.0f, 0.0f, 1.0f);

		gl.glDrawElements(GL10.GL_LINES, nrOfVertices,
				GL10.GL_UNSIGNED_SHORT, indexBuffer);

	}

}