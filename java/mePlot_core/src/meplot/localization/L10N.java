package meplot.localization;

import meplot.persistence.Settings;
import platform.persistence.Persistence;

public final class L10N {
	public static final int NO = 17;
	public static final int BOTH = 18;
	public static final int YES = 19;
	public static final int NONE = 20;
    public static final int OPERATIONS = 22;
    public static final int MATRICES = 24;
	public static final int DERIVATIONEXCEPTION = 25;
	public static final int ALGEBRA = 26;

    private L10N() {
		values = getValues();
	}

	private String[] getValues() {
        final int language = Persistence.loadInt(Settings.LANGUAGE);
        if (language == 0)
            return loadLanguage();
        return ENGLISH;
	}

	private String[] loadLanguage() {
        return ENGLISH;
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
