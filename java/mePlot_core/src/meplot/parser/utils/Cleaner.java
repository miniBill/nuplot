package meplot.parser.utils;

import meplot.expressions.operations.Operation;
import meplot.parser.ParserException;
import platform.log.Log;

public final class Cleaner{
	private static final String MINUS = "-";

	private Cleaner(){
	}

	public static String cleanInput(final String string){
		if(string.equals("+"))
			return "0";
		if(string.equals("*"))
			return "1";
		String toret = replace(string, "²", "^2");
		toret = replace(toret, "³", "^3");
		toret = cut(toret, "++", "+");
		toret = cut(toret, "+-", MINUS);
		toret = cut(toret, "-+", MINUS);
		toret = cut(toret, "--", "+");
		toret = cut(toret, "^-1", "^(-1)");
		toret = replace(toret, "'", "");
		if(toret.endsWith("="))
			toret = toret.substring(0, toret.length() - 1);
		try{
			for(int c = 0; c < Operation.OPERATIONS.length; c++){
				final String mapsTo = Operation.parseMapsTo(c);
				if(mapsTo.length() > 1)
					toret = cut(toret, mapsTo, Operation.OPERATIONS[c] + "");
			}
		}
		catch(final ParserException e){
			Log.log(e);
		}
		return toret;
	}

	public static String cut(final String string, final String search, final String replace){
		String toret = string;
		while(toret.indexOf(search) >= 0)
			toret = Cleaner.replace(toret, search, replace);
		return toret;
	}

	public static String clean(final String string){
		final String one = replace(string, "+-", MINUS);
		final String two = replace(one, "-+", MINUS);
		final String three = replace(two, "-1*", MINUS);
		return replace(three, "**", "^");
	}

	/**
	 * Replaces a text inside a string.
	 * 
	 * @author 
	 *         http://cf-sami.blogspot.com/2009/06/j2me-string-replace-method.html
	 * @param text
	 *            Input string.
	 * @param searchStr
	 *            String to search and replace.
	 * @param replacementStr
	 *            String to put as a replacement.
	 * @return The input string, with replacements done.
	 */
	public static String replace(final String text, final String searchStr, final String replacementStr){
		// String buffer to store str
		final StringBuffer sbuffer = new StringBuffer();

		// Search for search
		int searchStringPos = text.indexOf(searchStr);
		int startPos = 0;

		// Iterate to add string
		while(searchStringPos != -1){
			sbuffer.append(text.substring(startPos, searchStringPos)).append(replacementStr);
			startPos = searchStringPos + searchStr.length();
			searchStringPos = text.indexOf(searchStr, startPos);
		}

		// Create string
		sbuffer.append(text.substring(startPos, text.length()));

		return sbuffer.toString();
	}

	public static String dematrix(final String cString){
		final String try1 = tryBetween(cString, '<');
		if(try1 != null)
			return dematrix(try1);
		final String try2 = tryBetween(cString, Operation.LEQ);
		if(try2 != null)
			return dematrix(try2);
		if(cString.charAt(0) == '{' && cString.endsWith("}") && cString.lastIndexOf('{') == 0)
			return cString.substring(1, cString.length() - 1);
		return cString;
	}

	private static String tryBetween(final String cString, final char leq){
		final int left = cString.indexOf(leq);
		if(left > 0 && cString.length() > left + 4){
			final int right = cString.indexOf(leq, left + 1);
			final char leftLetter = cString.charAt(left + 1);
			final char rightLetter = cString.charAt(left + 3);
			if(right == left + 4 && leftLetter >= 'a' && leftLetter <= 'z' && leftLetter == rightLetter
					&& cString.charAt(left + 2) == ',')
				return cString.substring(0, left + 2) + cString.substring(left + 4);
		}
		return null;
	}
}
