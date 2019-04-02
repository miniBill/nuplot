package meplot.localization;

public final class L10N {
	public static final int DERIVATIONEXCEPTION = 25;

	private L10N() {
		values = ENGLISH;
	}

	private static final String[] ENGLISH = new String[] { "Exit", "Back", "Draw", "Add equation", "Del equation",
			"Insert", "Next", "Options", "Symbols", "Analyze", "Solver", "Solve", "Input matrix", "Define functions",
			"About", "Derivative", "Input", "No", "Both", "Yes", "None", "Solution", "Operations", "Boolean",
			"Matrices", "Derivation exception", "Algebra", "mePlot" };

	private final String[] values;

	private static final L10N INSTANCE = new L10N();

    public static String get(final int index) {
		if (index >= INSTANCE.values.length) {
			if (index >= ENGLISH.length)
				return index + "???";
			return ENGLISH[index] + "?!";
		}
		return INSTANCE.values[index];
	}

	public static int count() {
		return ENGLISH.length;
	}
}
