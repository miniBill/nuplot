package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.OdeGraph;
import meplot.numerical.RungeKutta;
import platform.lists.List;

import org.junit.Test;

public class PlotterTest extends TestUtils {
	@Test
	public void testOde() {
		final Expression expr = parseOrFail("ode(1-xx,0,0)");
		final OdeGraph graph = new OdeGraph(expr, 0xFF0000);
		final List<Graph> graphList = new List<>();
		graphList.add(graph);

		final RungeKutta rk = new RungeKutta(parseOrFail("{{0,1},{5,1}}x"), 0, parseOrFail("tr{3,0}"),
				parseOrFail("{1,0}"));
		rk.start(0.1);
		final StringBuilder sb = new StringBuilder();
		for (int c = 0; c < 3; c++) {
			sb.append(rk.next());
			if (c < 2)
				sb.append('\n');
		}
		assertEquals("RK-4 gone wrong", "{3}\n{3.0645625}\n{3.2956991810546876}", sb.toString());
	}

}
