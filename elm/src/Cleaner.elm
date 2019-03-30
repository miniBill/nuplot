module Cleaner exposing (cleanInput)

import Expression exposing (..)


operationsMap =
    [ ( "+", "+" )
    , ( "/", "/" )
    , ( "=", "=" )
    , ( ">=", "≥" )
    , ( ">", ">" )
    , ( "=>", "⇒" )
    , ( "<=", "≤" )
    , ( "<", "<" )
    , ( "%", "%" )
    , ( "*", "*" )
    , ( "<>", "≠" )
    , ( "^", "^" )
    ]


cleanInput string =
    let
        cut f t s =
            if s == String.replace f t s then
                s

            else
                cut f t (String.replace f t s)
    in
    case string of
        "+" ->
            "0"

        "*" ->
            "1"

        _ ->
            string
                |> String.replace "²" "^2"
                |> String.replace "³" "^3"
                |> String.replace "'" ""
                |> cut "++" "+"
                |> cut "+-" "-"
                |> cut "-+" "-"
                |> cut "--" "+"
                |> cut "^-1" "^(-1)"
                |> (\toret_8 ->
                        if String.endsWith "=" toret_8 then
                            String.dropRight 1 toret_8

                        else
                            toret_8
                   )
                |> (\r ->
                        List.foldl (\( ef, et ) -> cut ef et) r operationsMap
                   )



{-
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
-}
