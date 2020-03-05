module Parser exposing (parse)

import AssumptionParser
import Cleaner
import Expression exposing (Expression)
import FunctionActivator
import MatrixDivider
import OperationActivator
import ParserModel exposing (..)
import TokenList exposing (TokenIterator, TokenList)


type Token
    = CharToken Char


tokenize : String -> List Token
tokenize =
    String.toList
        >> List.filter ((/=) ' ')
        >> List.map CharToken



{-
       private static TokenList unfoldStrings(final TokenIterator iterator){
           final TokenList toret = new TokenList();
           while(iterator.hasNext()){
               final IToken current = iterator.next();
               if(current instanceof TokenList)
                   toret.add(unfoldStrings(((TokenList)current).getIterator()));
               else
                   if(current instanceof CharList)
                       toret.addRange(((CharList)current).getIterator());
                   else
                       toret.add(current);
           }
           return toret;
       }
       private static TokenList aggregateStrings(final TokenIterator iterator){
           final TokenList toret = new TokenList();
           while(iterator.hasNext()){
               IToken token = iterator.next();
               if(token instanceof TokenList)
                   token = aggregateStrings(((AbstractTokenList)token).getIterator());
               if(token instanceof CharToken){
                   final CharList charList = new CharList();
                   while(token instanceof CharToken){
                       charList.add(token);
                       if(iterator.hasNext())
                           token = iterator.next();
                       else{
                           toret.addRange(charList.aggregate());
                           return toret;
                       }
                   }
                   toret.addRange(charList.aggregate());
                   if(token instanceof TokenList)
                       token = aggregateStrings(((AbstractTokenList)token).getIterator());
               }
               toret.add(token);
           }
           return toret;
       }
       private static ITokenList processParenthesis(final TokenIterator iterator) throws ParserException{
           final ITokenList toret = new TokenList();
           while(iterator.hasNext()){
               final IToken curr = iterator.next();
               if(curr instanceof ParToken){
                   final ParToken partoken = (ParToken)curr;
                   if(partoken.isOpen()){
                       if(!iterator.hasNext())
                           throw new ParserException();
                       final ITokenList temp = processParenthesis(iterator);
                       toret.add(temp);
                   }
                   else
                       break;
               }
               else
                   toret.add(curr);
           }
           return toret;
       }
       private static ITokenList aggregateNumbers(final TokenIterator iterator, final boolean exact)
               throws ParserException{
           final ITokenList root = new TokenList();
           while(iterator.hasNext()){
               IToken curr = iterator.next();
               char currChar = curr.toString().charAt(0);
               if(isDigit(currChar)){
                   final ITokenList temp = new TokenList();
                   boolean appendIt = false;
                   boolean isDouble = false;
                   while(isDigit(currChar)){
                       if(currChar == '.' || currChar == '@')
                           isDouble = true;
                       temp.add(curr);
                       appendIt = false;
                       if(iterator.hasNext()){
                           curr = iterator.next();
                           currChar = curr.toString().charAt(0);
                           appendIt = true;
                       }
                       else
                           break;
                   }
                   if(isDouble)
                       if(exact)
                           root.add(new PerfectDoubleToken(temp));
                       else
                           root.add(new DoubleToken(temp));
                   else
                       root.add(new IntToken(temp));
                   if(appendIt)
                       root.add(curr);
               }
               else
                   root.add(curr);
           }
           return root;
       }
       private static boolean isDigit(final char currChar){
           return currChar >= '0' && currChar <= '9' || currChar == '.' || currChar == '@';
       }
       private static ITokenList activateSyntax(final TokenIterator iterator){
           final ITokenList toret = new TokenList();
           // third pass: activate operators and parenthesis
           while(iterator.hasNext()){
               final IToken curr = iterator.next();
               if(curr instanceof CharToken){
                   final char val = curr.toString().charAt(0);
                   boolean found = false;
                   for(int c = 0; c < Operation.OPERATIONS.length; c++)
                       if(val == Operation.OPERATIONS[c]){
                           final OperationToken optok = new OperationToken(val);
                           toret.add(optok);
                           found = true;
                           break;
                       }
                   if(!found)
                       switch(val){
                           case '(':
                               final ParToken par = new ParToken(true);
                               toret.add(par);
                               break;
                           case ')':
                               final ParToken cpar = new ParToken(false);
                               toret.add(cpar);
                               break;
                           case '-':
                               activateMinus(toret);
                               break;
                           default:
                               toret.add(curr);
                               break;
                       }
               }
               else
                   toret.add(curr);
           }
           return toret;
       }
       private static void activateMinus(final ITokenList toret){
           final IToken last = toret.getLast();
           if(last instanceof OperationToken){
               final char val = ((OperationToken)last).getVal();
               if(val == Operation.DIVISION || val == Operation.MULTIPLICATION){
                   toret.add(new FunctionToken("M", new Token[0]));
                   return;
               }
           }
           toret.add(new OperationToken(Operation.ADDITION));
           toret.add(new IntToken(-1));
           toret.add(new OperationToken(Operation.MULTIPLICATION));
       }
       public static Expression parseOrDefault(final String test, final Expression zero){
           try{
               return parse(test);
           }
           catch(final ParserException e){
               Log.log(LogLevel.WARNING, "Parse error: " + e.getMessage() + " | " + e.toString());
               return zero;
           }
       }
       public static Expression parseOrDefault(final String string){
           return parseOrDefault(string, Int.ZERO);
       }
       public static UserFunction parseUserFunction(final String string) throws ParserException{
           final int semicolon = string.indexOf(':');
           final int opened = string.indexOf('(');
           if(opened < 0 || semicolon < 0)
               throw new ParserException("':' or '(' missing in function definition");
           final String name = string.substring(0, opened);
           final String argString = string.substring(opened + 1, semicolon);
           final int num = argString.length() / 2;
           final char[] vars = new char[num];
           for(int c = 0; c < num; c++)
               vars[c] = argString.charAt(c * 2);
           final String rest = string.substring(semicolon + 2);
           final Expression expr = parseOrDefault(rest);
           return new UserFunction(num, name, vars, expr);
       }
   }

-}


aggregateNumbers : TokenIterator -> TokenList
aggregateNumbers =
    Debug.todo "aggregateNumbers"


activateSyntax : TokenIterator -> TokenList
activateSyntax =
    Debug.todo "activateSyntax"


processParenthesis : TokenIterator -> TokenList
processParenthesis =
    Debug.todo "processParenthesis"


aggregateStrings : TokenIterator -> TokenList
aggregateStrings =
    Debug.todo "aggregateStrings"


unfoldStrings : TokenIterator -> TokenList
unfoldStrings =
    Debug.todo "unfoldStrings"


parse : String -> Result ParserError Expression
parse string =
    case string of
        "" ->
            Err EmptyInput

        _ ->
            if String.contains "[" string then
                AssumptionParser.processAssumptions parse string

            else if String.contains ";" string then
                parse <| "{" ++ String.replace ";" "," string ++ "}"

            else
                let
                    parsing =
                        Cleaner.cleanInput string

                    root =
                        tokenize parsing

                    divided =
                        MatrixDivider.divideMatrices root
                in
                divided.rest
                    |> aggregateNumbers
                    |> TokenList.getIterator
                    |> activateSyntax
                    |> TokenList.getIterator
                    |> processParenthesis
                    |> TokenList.getIterator
                    |> aggregateStrings
                    |> TokenList.getIterator
                    |> unfoldStrings
                    |> TokenList.getIterator
                    |> FunctionActivator.activateFunctions
                    |> OperationActivator.activateOperations
                    |> TokenList.toExpression
                    |> (\out -> List.foldl (\( letter, expr ) -> Expression.partialSubstitute letter expr) out divided.iterator)
