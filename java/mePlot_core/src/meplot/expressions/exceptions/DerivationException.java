package meplot.expressions.exceptions;

public final class DerivationException extends CalcException {
	private static final long serialVersionUID = 4070968519186639307L;

	public DerivationException() {
		super("Derivation exception");
	}

	public DerivationException(final String message) {
		super(message);
	}
}
