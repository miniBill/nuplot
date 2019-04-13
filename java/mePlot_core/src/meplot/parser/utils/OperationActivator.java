package meplot.parser.utils;

import meplot.expressions.operations.Operation;
import meplot.parser.ParserException;
import meplot.parser.tokens.*;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.Myterator;
import platform.log.Log;
import platform.log.LogLevel;

import java.util.Iterator;

public final class OperationActivator {
	private OperationActivator() {
	}

	public static ITokenList activateOperations(final Iterable<IToken> iterable) throws ParserException {
		ITokenList it2 = activateOperation(iterable.iterator(), Operation.POWER);
		for (char operation : Operation.OPERATIONS)
			if (operation != Operation.POWER) {
				final ITokenList tokenList = activateOperation(it2.iterator(), operation);
				Log.log(LogLevel.PARSER, "After activating " + operation + ':', tokenList.toString());
				it2 = tokenList;
			}
		return it2;
	}

	private static ITokenList activateOperation(final Iterator<IToken> iterator, final char operation)
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
						if (((Myterator)iterator).isSecond()) {
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
		final IList<IToken> args = IterableExtensions.toList(token.getValues());
		final IToken[] newargs = new IToken[args.length()];
		for (int c = 0; c < args.length(); c++)
			newargs[c] = activateToken(args.elementAt(c), operation);
		return token.fill(newargs);
	}
}
