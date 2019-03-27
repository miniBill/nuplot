package meplot.graphics.graphs;

import platform.lists.List;

public final class GraphList extends List{
	public GraphList getSinglet(){
		final GraphList toret = new GraphList();
		if(!isEmpty())
			toret.add(getFirst());
		return toret;
	}

	public void add(final Graph graph){
		super.add(graph);
	}

	public Graph getFirst(){
		return (Graph)ggetFirst();
	}

	public Graph elementAt(final int index){
		return (Graph)gelementAt(index);
	}

	public GraphIterator getIterator(){
		return new GraphIterator(this);
	}
}
