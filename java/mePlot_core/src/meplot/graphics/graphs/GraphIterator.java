package meplot.graphics.graphs;

import platform.lists.IIterable;
import platform.lists.Iterator;

public final class GraphIterator extends Iterator{
	private final GraphList inner;

	public GraphIterator(final GraphList head){
		inner = head;
	}

	public GraphIterator(final GraphList head, final int index){
		super(index);
		inner = head;
	}

	public Graph next(){
		return inner.elementAt(index++);
	}

	public GraphIterator subIterator(){
		return new GraphIterator(inner, index);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
