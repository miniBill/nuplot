package meplot.expressions.operations;

import meplot.parser.ParserException;

public final class Operation {
	public static final char ADDITION = '+';
	private static final String ADDITION_PARSE = "+";
	public static final char DIVISION = '/';
	private static final String DIVISION_PARSE = "/";
	public static final char EQUALS = '=';
	private static final String EQUALS_PARSE = "=";
	public static final char GEQ = '≥';
	private static final String GEQ_PARSE = ">=";
	public static final char GREATER = '>';
	private static final String GREATER_PARSE = ">";
	public static final char LAMBDA = '⇒';
	private static final String LAMBDA_PARSE = "=>";
	public static final char LEQ = '≤';
	private static final String LEQ_PARSE = "<=";
	public static final char LESS = '<';
	private static final String LESS_PARSE = "<";
	public static final char MOD = '%';
	private static final String MOD_PARSE = "%";
	public static final char MULTIPLICATION = '*';
	private static final String MULTIPLICATION_PARSE = "*";
	public static final char NEQ = '≠';
	private static final String NEQ_PARSE = "<>";
	public static final char POWER = '^';
	private static final String POWER_PARSE = "^";
	public static final char UNKNOWN = '?';

	public static final char[] OPERATIONS = new char[] { LEQ, GEQ, NEQ, LESS, LAMBDA, GREATER, EQUALS, ADDITION,
			MULTIPLICATION, DIVISION, POWER, MOD };
	private static final String[] PARSE_MAPPING = new String[] { LEQ_PARSE, GEQ_PARSE, NEQ_PARSE, LESS_PARSE,
			LAMBDA_PARSE, GREATER_PARSE, EQUALS_PARSE, ADDITION_PARSE, MULTIPLICATION_PARSE, DIVISION_PARSE,
			POWER_PARSE, MOD_PARSE };

	private Operation() {
	}

	public static String parseMapsTo(int val) throws ParserException {
		if (val < 0 || val > 11)
			throw new ParserException("Asked for unknown operation number");
		return PARSE_MAPPING[val];
	}
}
