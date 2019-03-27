package meplot.localization;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

import meplot.persistence.Settings;
import platform.Platform;
import platform.persistence.Persistence;

public final class L10N{
	public static final int EXIT = 0;
	public static final int BACK = 1;
	public static final int DRAW = 2;
	public static final int ADDEQUATION = 3;
	public static final int DELEQUATION = 4;
	public static final int INSERT = 5;
	public static final int NEXT = 6;
	public static final int OPTIONS = 7;
	public static final int SYMBOLS = 8;
	public static final int ANALYZE = 9;
	public static final int SOLVER = 10;
	public static final int SOLVE = 11;
	public static final int INPUTMATRIX = 12;
	public static final int DEFINEFUNCTIONS = 13;
	public static final int ABOUT = 14;
	public static final int DERIVATIVE = 15;
	public static final int INPUT = 16;
	public static final int NO = 17;
	public static final int BOTH = 18;
	public static final int YES = 19;
	public static final int NONE = 20;
	public static final int SOLUTION = 21;
	public static final int OPERATIONS = 22;
	public static final int BOOLEAN = 23;
	public static final int MATRICES = 24;
	public static final int DERIVATIONEXCEPTION = 25;
	public static final int ALGEBRA = 26;
	public static final int MAINTITLE = 27;

	private L10N(){
		values = getValues();
	}

	private String[] getValues(){
		try{
			final int language = Persistence.loadInt(Settings.LANGUAGE);
			if(language == 0)
				return loadLanguage("it");
		}
		catch(final IOException e){
		}
		return ENGLISH;
	}

	private String[] loadLanguage(final String lang) throws IOException{
		if(!Platform.isJ2ME())
			return ENGLISH;
		return loadText('/' + lang + ".res");
	}

	private static final String[] ENGLISH = new String[]{"Exit", "Back", "Draw",
			"Add equation", "Del equation", "Insert", "Next", "Options", "Symbols",
			"Analyze", "Solver", "Solve", "Input matrix", "Define functions", "About",
			"Derivative", "Input", "No", "Both", "Yes", "None", "Solution", "Operations",
			"Boolean", "Matrices", "Derivation exception", "Algebra", "mePlot"};

	private final String[] values;

	private static final L10N ISTANCE = new L10N();

	private String[] loadText(final String resName) throws IOException{
		final InputStream stream = getClass().getResourceAsStream(resName);
		final DataInputStream din = new DataInputStream(stream);
		if(stream == null)
			throw new IOException();
		try{
			final int size = din.readShort();
			final String[] toret = new String[size];
			for(int i = 0; i < size; i++)
				toret[i] = din.readUTF();
			return toret;
		}
		finally{
			din.close();
			stream.close();
		}
	}

	public static String get(final int index){
		if(index >= ISTANCE.values.length){
			if(index >= ENGLISH.length)
				return Integer.toString(index) + "???";
			return ENGLISH[index] + "?!";
		}
		return ISTANCE.values[index];
	}

	public static int count(){
		return ENGLISH.length;
	}
}
