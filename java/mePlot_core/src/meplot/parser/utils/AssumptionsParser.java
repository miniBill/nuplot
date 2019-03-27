package meplot.parser.utils;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.list.IValueList;
import meplot.expressions.list.ValueList;
import meplot.expressions.list.ValueNode;
import meplot.parser.Parser;
import meplot.parser.ParserException;

public final class AssumptionsParser{
	private AssumptionsParser(){
	}

	public static Expression processAssumptions(final String string)
			throws ParserException{
		final int close = string.indexOf(']');
		if(close < 0)
			throw new ParserException();
		final int open = string.indexOf('[');
		if(open != 0 && string.charAt(0) != '\'')
			throw new ParserException("'[' not in first position");
		if(close == open + 1)
			return Parser.parse(string.substring(2));
		final String assume = string.substring(open + 1, close);
		final String rest = string.substring(close + 1);
		final IValueList assumed = parseAssumptions(assume);
		final ISubstitutible parsed = Parser.parse(rest);
		return parsed.partialSubstitute(assumed);
	}

	private static IValueList parseAssumptions(final String assume)
			throws ParserException{
		final IValueList toret = new ValueList();
		parseAssumptions(assume, toret);
		return toret;
	}

	/**
	 * WARNING: modifies the toret parameter.
	 *
	 * @throws ParserException
	 */
	private static void parseAssumptions(final String inner, final IValueList toret)
			throws ParserException{
		final int semicolon = inner.indexOf(';');
		String first = inner;
		String rest = "";
		if(semicolon != -1){
			first = inner.substring(0, semicolon);
			rest = inner.substring(semicolon + 1);
		}
		toret.add(parseAssumption(first));
		if(semicolon < 0)
			return;
		parseAssumptions(rest, toret);
	}

	private static ValueNode parseAssumption(final String inner) throws ParserException{
		final char letter = inner.charAt(0);
		if(inner.length() == 1 || inner.charAt(1) == '=' && inner.length() == 2)
			throw new ParserException("Valueless assumption");
		final Expression value = Parser.parse(inner.charAt(1) == '=' ? inner.substring(2)
				: inner.substring(1));
		return new ValueNode(letter, value);
	}
}
