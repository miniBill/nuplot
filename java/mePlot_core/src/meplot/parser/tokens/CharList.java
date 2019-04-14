package meplot.parser.tokens;

public final class CharList extends AbstractTokenList {
	/**
	 * Creates an empty list.
	 */
	public CharList() {
		// Creates an empty list.
	}

	public CharList(final String substring) {
		for (int c = 0; c < substring.length(); c++)
			add(new CharToken(substring.charAt(c)));
	}

	public String toString() {
		final StringBuilder toret = new StringBuilder("{C");
		cString(toret);
		toret.append('}');
		return toret.toString();
	}

	private String toSmallString() {
		final StringBuilder toret = new StringBuilder();
		for (IToken iToken : this)
			toret.append(iToken);
		return toret.toString();
	}

	public ITokenList aggregate() {
		final String small = toSmallString();
		return FunctionToken.parse(small);
	}
}
