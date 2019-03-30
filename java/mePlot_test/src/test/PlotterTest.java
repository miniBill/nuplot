package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.graphics.AbstractGraphics;
import meplot.graphics.DrawController;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.NormalGraph;
import meplot.graphics.graphs.OdeGraph;
import meplot.numerical.RungeKutta;
import platform.lists.List;

import org.junit.Test;

public class PlotterTest extends TestUtils {
	@Test
	public void testOde() {
		final DrawController controller = new DrawController();
		final AbstractGraphics graphics = new StringGraphics();
		final Expression expr = parseOrFail("ode(1-xx,0,0)");
		final OdeGraph graph = new OdeGraph(expr, 0xFF0000);
		final List<Graph> graphList = new List<Graph>();
		graphList.add(graph);
		try {
			controller.doPaint(graphics, graphList, 100, 100).join();
		} catch (final InterruptedException e) {
			fail("WTH?");
		}

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

	@Test
	public void testXP() {
		final DrawController dc = new DrawController();
		checkNormalGraph(dc, "x+1",
				"C(0)R(0,0,10,10)C(3158080)L(0,0,0,10)L(1,0,1,10)"
						+ "L(2,0,2,10)L(3,0,3,10)L(4,0,4,10)L(5,0,5,10)L(6,0,6,10)L(7,0,7,10)"
						+ "L(8,0,8,10)L(9,0,9,10)L(10,0,10,10)L(0,10,10,10)L(0,9,10,9)"
						+ "L(0,8,10,8)L(0,7,10,7)L(0,6,10,6)L(0,5,10,5)L(0,4,10,4)L(0,3,10,3)"
						+ "L(0,2,10,2)L(0,1,10,1)L(0,0,10,0)C(16777215)S(4.0)V(0)"
						+ "T(0,5,-5.0)T(1,5,-4.0)T(2,5,-3.0)T(3,5,-2.0)T(4,5,-1.0)"
						+ "T(5,5,0.0)T(6,5,1.0)T(7,5,2.0)T(8,5,3.0)T(9,5,4.0)T(10,5,5.0)"
						+ "H(0)T(5,10,-5.0)T(5,9,-4.0)T(5,8,-3.0)T(5,7,-2.0)T(5,6,-1.0)"
						+ "T(5,5,0.0)T(5,4,1.0)T(5,3,2.0)T(5,2,3.0)T(5,1,4.0)T(5,0,5.0)"
						+ "L(5,0,5,10)L(0,5,10,5)C(16711680)C(16711680)L(0,9,1,8)L(1,8,2,7)"
						+ "L(2,7,3,6)L(3,6,4,5)L(4,5,5,4)L(5,4,6,3)L(6,3,7,2)L(7,2,8,1)"
						+ "L(8,1,9,0)C(16711680)L(1,1,1,1)C(65280)L(1,2,1,2)C(255)L(1,3,1,3)" + "C(0)F()");
	}

	private static void checkNormalGraph(final DrawController controller, final String function, final String result) {
		final AbstractGraphics graphics = new StringGraphics();
		final ISubstitutible expr = parseOrFail(function);
		final NormalGraph graph = new NormalGraph(expr, 0xFF0000);
		final List<Graph> graphList = new List<Graph>();
		graphList.add(graph);
		try {
			controller.doPaint(graphics, graphList, 10, 10).join();
		} catch (final InterruptedException e) {
			fail("WTH?");
		}
		assertEquals("Graphic for " + function + " is not the same", result, graphics.toString());
	}
}
