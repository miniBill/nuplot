package meplot.expressions.exceptions;

import meplot.localization.L10N;

public final class DerivationException extends CalcException {
	private static final long serialVersionUID = 4070968519186639307L;

	public DerivationException() {
		super(L10N.get(L10N.DERIVATIONEXCEPTION));
	}

	public DerivationException(final String message) {
		super(message);
	}
}
