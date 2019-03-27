package meplot.graphics.graphs;

abstract class AbstractGraph implements Graph{
	private final int color;

	protected AbstractGraph(final int color){
		this.color = color;
	}

	public final int getColor(){
		return color;
	}
}
