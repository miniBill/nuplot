package meplot.graphics.graphs;

import meplot.expressions.numbers.Int;

public interface Graph{
	Graph EMPTY = new NormalGraph(Int.ZERO, 0xFFFFFF);

	boolean isRadial();

	boolean is3D();

	boolean isImplicit();

	int getColor();
}
