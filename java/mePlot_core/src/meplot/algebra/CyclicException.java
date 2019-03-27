package meplot.algebra;

import meplot.expressions.exceptions.CalcException;

final class CyclicException extends CalcException {
	private static final long serialVersionUID = 6720750037700226857L;

	CyclicException(final String message) {
		super(message);
	}
}
