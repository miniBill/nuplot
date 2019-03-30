module MatrixDivider exposing (divideMatrices)


divideMatrices root =
    let
        state =
            { toret = { matrices = [], rest = [] }, name = 'A' }
    in
    Debug.todo "divideMatrices"



{-
       private static SymbolListener listener;

       public static Divided divideMatrices(final ITokenList root) throws ParserException{
           final Divided toret = new Divided();
           final TokenIterator iterator = root.getIterator();
           char name = 'A';
           while(iterator.hasNext()){
               IToken curr = iterator.next();
               if("{".equals(curr.toString())){
                   if(!iterator.hasNext())
                       throw new ParserException("Error in matrix parse: wrong size!");
                   if("{".equals(iterator.peek().toString())){
                       curr = processColumn(iterator);
                       try{
                           final Expression expr = curr.toExpression();
                           toret.add(name, expr);
                       }
                       catch(final ArrayIndexOutOfBoundsException e){
                           throw new ParserException("Error in matrix parse: wrong size?", e);
                       }
                       toret.rest().add(new CharToken(name++));
                   }
                   else{
                       curr = processRow(iterator);
                       final Expression expr = curr.toExpression();
                       if(listener != null)
                           listener.addMatrix(expr.toString());
                       toret.add(name, expr);
                       toret.rest().add(new CharToken(name++));
                   }
               }
               else
                   toret.rest().add(curr);
           }
           return toret;
       }

       private static MatrixTokenList processColumn(final TokenIterator iterator){
           final MatrixTokenList toret = new MatrixTokenList();
           while(iterator.hasNext()){
               iterator.next(); // skip "{"
               final IToken curr = processRow(iterator);
               toret.add(curr);
               if(iterator.hasNext()){
                   if("}".equals(iterator.peek().toString())){
                       iterator.next(); // drop }
                       return toret;
                   }
                   if(iterator.hasNext())
                       iterator.next(); // jump comma
               }
           }
           return toret;
       }

       private static MatrixTokenList processRow(final TokenIterator iterator){
           final MatrixTokenList toret = new MatrixTokenList();
           TokenList temp = new TokenList();
           while(iterator.hasNext()){
               if(iterator.peek().toString().equals("}")){
                   toret.add(temp);
                   iterator.next(); // discard }
                   return toret;
               }
               final IToken curr = iterator.next();
               if(",".equals(curr.toString())){
                   toret.add(temp);
                   temp = new TokenList();
               }
               else
                   temp.add(curr);
           }
           toret.add(temp);
           return toret;
       }

       public static void setListener(final SymbolListener newlistener){
           MatrixDivider.listener = newlistener;
       }
   }

-}
