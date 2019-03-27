package meplot.daplot.gab.plotter;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

import javax.microedition.khronos.opengles.GL10;

public final class SimpleMesh{
	private ShortBuffer indexBuffer;
	private FloatBuffer vertexBuffer;
	private FloatBuffer colorBuffer;
	private FloatBuffer textureUVWBuffer;
	private int nrOfVertices;

	public static float setColor(final int color){
		if(color < 0)
			return 0;
		if(color > 255)
			return 1;
		return color / 255F;
	}

	public SimpleMesh(final short[] indices, final float[] vertices,
			final float[] colors, final float[] texture){
		buildBuffers(indices, vertices, colors, texture);
	}

	public void buildBuffers(final short[] indices, final float[] vertices,
			final float[] colors, final float[] texture){

		// short has 2 bytes, indices * 2 bytes
		final ByteBuffer ibb = ByteBuffer.allocateDirect(indices.length * 2);
		ibb.order(ByteOrder.nativeOrder());
		indexBuffer = ibb.asShortBuffer();
		nrOfVertices = indices.length;

		// float has 4 bytes, coordinate * 4 bytes
		final ByteBuffer vbb = ByteBuffer.allocateDirect(vertices.length * 4);
		vbb.order(ByteOrder.nativeOrder());
		vertexBuffer = vbb.asFloatBuffer();
		// nrOfVertices=vertices.length;

		// float has 4 bytes, colors (RGBA) * 4 bytes
		final ByteBuffer cbb = ByteBuffer.allocateDirect(colors.length * 4);
		cbb.order(ByteOrder.nativeOrder());
		colorBuffer = cbb.asFloatBuffer();

		// float has 4 bytes, colors (UVW) * 4 bytes
		final ByteBuffer tbb = ByteBuffer.allocateDirect(texture.length * 4);
		tbb.order(ByteOrder.nativeOrder());
		textureUVWBuffer = tbb.asFloatBuffer();

		indexBuffer.put(indices);
		vertexBuffer.put(vertices);
		colorBuffer.put(colors);
		textureUVWBuffer.put(texture);

		indexBuffer.position(0);
		vertexBuffer.position(0);
		colorBuffer.position(0);
		textureUVWBuffer.position(0);

	}

	public void draw(final GL10 gl, final float[] pos, final float[] scale,
			final float[] rot){

		gl.glVertexPointer(3, GL10.GL_FLOAT, 0, vertexBuffer);
		gl.glColorPointer(4, GL10.GL_FLOAT, 0, colorBuffer);
		gl.glTexCoordPointer(3, GL10.GL_FLOAT, 0, textureUVWBuffer);

		gl.glLoadIdentity();

		gl.glScalef(scale[0], scale[1], scale[2]);
		gl.glTranslatef(pos[0], pos[1], pos[2]);

		gl.glRotatef(rot[0], 1f, 0f, 0f);
		gl.glRotatef(rot[1], 0f, 1f, 0f);
		gl.glRotatef(rot[2], 0f, 0f, 1f);

		gl.glDrawElements(GL10.GL_TRIANGLES, nrOfVertices,
				GL10.GL_UNSIGNED_SHORT, indexBuffer);

	}

	public void draw(final GL10 gl, final float[] matrix){
		gl.glVertexPointer(3, GL10.GL_FLOAT, 0, vertexBuffer);
		gl.glColorPointer(4, GL10.GL_FLOAT, 0, colorBuffer);
		gl.glTexCoordPointer(3, GL10.GL_FLOAT, 0, textureUVWBuffer);

		gl.glLoadIdentity();

		gl.glLoadMatrixf(matrix, 0);

		gl.glDrawElements(GL10.GL_TRIANGLES, nrOfVertices,
				GL10.GL_UNSIGNED_SHORT, indexBuffer);
	}

}
