package meplot.parser;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.graphics.graphs.Graph;
import meplot.graphics.graphs.NormalGraph;
import meplot.graphics.graphs.OdeGraph;
import meplot.graphics.graphs.ParametricGraph;
import meplot.graphics.graphs.ThreeParametricGraph;

public final class GraphParser{
	private GraphParser(){
	}

	private static Graph normalParse(final String string, final int color)
			throws ParserException{
		if(string.startsWith("gode") || string.startsWith("ode"))
			return new OdeGraph(Parser.parse(string), color);
		final ISubstitutible expr = Parser.parse(string);
		return new NormalGraph(expr, color);
	}

	private static Graph parametricParse(final String string, final int color,
			final String assume) throws ParserException{
		final int splitPoint = string.indexOf(';');

		final String xstring = string.substring(0, splitPoint);
		final Expression xexpr = Parser.parse(assume + xstring);

		final String rest = string.substring(splitPoint + 1, string.length());
		final int splitPoint2 = rest.indexOf(';');
		if(splitPoint2 < 0){ // 2D parametric
			final ISubstitutible yexpr = Parser.parse(assume + rest);
			if(xexpr.hasLetter('y'))
				return new ParametricGraph(yexpr, xexpr, color);
			return new ParametricGraph(xexpr, yexpr, color);
		}
		final String ystring = rest.substring(0, splitPoint2);
		final String zstring = rest.substring(splitPoint2 + 1, rest.length());
		final ISubstitutible yexpr = Parser.parse(assume + ystring);
		final ISubstitutible zexpr = Parser.parse(assume + zstring);
		return new ThreeParametricGraph(xexpr, yexpr, zexpr, color);
	}

	private static boolean isParametric(final String string){
		return string.indexOf(';') > string.indexOf(']');
	}

	public static Graph parse(final String string, final int color)
			throws ParserException{
		final int opened = string.indexOf('[');
		if(opened != -1){
			final int closed = string.indexOf(']');
			if(closed < 0)
				return null;
			final String rest = string.substring(closed + 1);
			if(isParametric(rest)){
				final String assume = string.substring(opened, closed + 1);
				return parametricParse(rest, color, assume);
			}
		}
		if(isParametric(string))
			return parametricParse(string, color);
		return normalParse(string, color);
	}

	private static Graph parametricParse(final String string, final int color)
			throws ParserException{
		return parametricParse(string, color, "");
	}
}
