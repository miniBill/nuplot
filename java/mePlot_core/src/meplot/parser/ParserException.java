package meplot.parser;

public final class ParserException extends Exception {
	private static final long serialVersionUID = -3510673627227450060L;

	private static final String PARSE_FAILED = "Parse failed: ";

	public ParserException() {
		super(PARSE_FAILED + '?');
		inner = null;
		msg = "";
	}

	private final/* @Nullable */Exception inner;

	private final String msg;

	public ParserException(final String message) {
		super(PARSE_FAILED + message);
		msg = message;
		inner = null;
	}

	public ParserException(final String string, final Exception exc) {
		super(PARSE_FAILED + string);
		msg = string;
		inner = exc;
	}

	public String toString() {
		if (inner == null)
			return PARSE_FAILED + msg;
		return PARSE_FAILED + msg + '[' + inner.toString() + "].";
	}
}
