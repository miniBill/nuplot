package meplot.expressions.functions;

import meplot.localization.L10N;

public final class FunctionCategory{
	public static final String OTHER = "Other";
	public static final String PIECEWISE = "Piecewise";
	public static final String POWER = "Power";
	public static final String OPERATIONS = L10N.get(L10N.OPERATIONS);
	public static final String USER_DEFINED = "User defined";
	public static final String COMPLEX = "Complex";
	public static final String TRIGONOMETRY = "Trigonometry";
	public static final String MATRIX = L10N.get(L10N.MATRICES);
	public static final String ALGEBRA = L10N.get(L10N.ALGEBRA);

	public static String[] getAll(){
		return new String[]{OPERATIONS, COMPLEX, TRIGONOMETRY, MATRIX, PIECEWISE, POWER,
				OTHER, USER_DEFINED, ALGEBRA};
	}

	private FunctionCategory(){
	}
}
