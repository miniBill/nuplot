package meplot.parser.utils;

import meplot.expressions.operations.Operation;
import meplot.parser.ParserException;
import meplot.parser.tokens.AbstractTokenList;
import meplot.parser.tokens.FunctionToken;
import meplot.parser.tokens.IToken;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.OperationToken;
import meplot.parser.tokens.Token;
import meplot.parser.tokens.TokenIterator;
import meplot.parser.tokens.TokenList;
import platform.lists.IIterator;
import platform.log.Log;
import platform.log.LogLevel;

public final class OperationActivator {
	private OperationActivator() {
	}

	public static TokenIterator activateOperations(final TokenIterator iterator) throws ParserException {
		final ITokenList ttl = activateOperation(iterator, Operation.POWER);
		TokenIterator it2 = ttl.titerator();
		for (int op = 0; op < Operation.OPERATIONS.length; op++)
			if (Operation.OPERATIONS[op] != Operation.POWER) {
				final ITokenList tokenList = activateOperation(it2, Operation.OPERATIONS[op]);
				Log.log(LogLevel.PARSER, "After activating " + Operation.OPERATIONS[op] + ':', tokenList.toString());
				it2 = tokenList.titerator();
			}
		return it2;
	}

	private static ITokenList activateOperation(final IIterator<IToken> iterator, final char operation)
			throws ParserException {
		TokenList temp = new TokenList();
		while (iterator.hasNext()) {
			IToken curr = iterator.next();
			if (curr instanceof AbstractTokenList)
				temp.add(activateToken(curr, operation));
			else {
				if (curr instanceof OperationToken)
					if (((OperationToken) curr).getVal() == operation) {
						final OperationToken opcurr = (OperationToken) curr;
						if (iterator.isSecond()) {
							if (opcurr.getVal() == Operation.ADDITION)
								continue;
							throw new ParserException();
						}
						if (opcurr.getVal() == Operation.ADDITION) {
							final IToken last = temp.getLast();
							if (last instanceof OperationToken && ((OperationToken) last).getVal() != Operation.POWER)
								continue;
						}
						if (!iterator.hasNext())
							throw new ParserException();
						final IToken left;
						if (operation == Operation.POWER)
							left = temp.pop();
						else {
							left = temp;
							temp = new TokenList();
						}

						final IToken right;
						if (operation == Operation.DIVISION || operation == Operation.POWER)
							right = activateToken(iterator.next(), operation);
						else
							right = activateOperation(iterator, operation);
						opcurr.setLeft(left);
						opcurr.setRight(right);
					} else
						curr = activateToken(curr, operation);
				else if (curr instanceof FunctionToken)
					curr = activateToken(curr, operation);
				temp.add(curr);
			}
		}
		return temp;
	}

	private static Token activateOperationToken(final OperationToken token, final char operation)
			throws ParserException {
		IToken left = token.getLeft();
		left = activateToken(left, operation);
		token.setLeft(left);

		IToken right = token.getRight();
		right = activateToken(right, operation);
		token.setRight(right);

		return token;
	}

	private static IToken activateToken(final IToken token, final char operation) throws ParserException {
		if (token instanceof AbstractTokenList)
			return activateOperation(((AbstractTokenList) token).iterator(), operation);
		else if (token instanceof OperationToken)
			return activateOperationToken((OperationToken) token, operation);
		else if (token instanceof FunctionToken)
			return activateOperationOnFunction((FunctionToken) token, operation);
		return token;
	}

	private static IToken activateOperationOnFunction(final FunctionToken token, final char operation)
			throws ParserException {
		if (token.getValues() == null)
			return token;
		final IToken[] args = token.getValues();
		final IToken[] newargs = new IToken[args.length];
		for (int c = 0; c < args.length; c++)
			newargs[c] = activateToken(args[c], operation);
		return token.fill(newargs);
	}
}
