package meplot.parser;

import meplot.expressions.Expression;
import meplot.expressions.functions.UserFunction;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Operation;
import meplot.parser.tokens.AbstractTokenList;
import meplot.parser.tokens.CharList;
import meplot.parser.tokens.CharToken;
import meplot.parser.tokens.DoubleToken;
import meplot.parser.tokens.FunctionToken;
import meplot.parser.tokens.IToken;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.IntToken;
import meplot.parser.tokens.OperationToken;
import meplot.parser.tokens.ParToken;
import meplot.parser.tokens.PerfectDoubleToken;
import meplot.parser.tokens.Token;
import meplot.parser.tokens.TokenIterator;
import meplot.parser.tokens.TokenList;
import meplot.parser.utils.AssumptionsParser;
import meplot.parser.utils.Cleaner;
import meplot.parser.utils.FunctionActivator;
import meplot.parser.utils.MatrixDivider;
import meplot.parser.utils.OperationActivator;
import platform.lists.IIterator;
import platform.log.Log;
import platform.log.LogLevel;

public final class Parser {
	private Parser() {
	}

	public static Expression parse(final String string) throws ParserException {
		if (string.length() == 0)
			throw new ParserException();

		final boolean exact = string.charAt(0) == '\'' || string.indexOf('@') >= 0;

		return parse(string, exact);
	}

	private static Expression parse(final String string, final boolean exact) throws ParserException {
		if (string.length() == 0)
			throw new ParserException();

		if (string.indexOf('[') != -1)
			return AssumptionsParser.processAssumptions(string);

		if (string.indexOf(';') != -1)
			return parse('{' + string.replace(';', ',') + '}', exact);

		final String parsing = Cleaner.cleanInput(string);

		ITokenList root = tokenize(parsing);

		final Divided divided = MatrixDivider.divideMatrices(root);

		root = divided.rest();

		// Log.log(LogLevel.PARSER, "After Tokenization:", root.toString());

		root = aggregateNumbers(root.getIterator(), exact);

		// Log.log(LogLevel.PARSER, "After aggregateNumbers:", root.toString());

		root = activateSyntax(root.getIterator());

		// Log.log(LogLevel.PARSER, "After activateSyntax:", root.toString());

		// from this step onward the structure is no more linear
		root = processParenthesis(root.getIterator());

		// Log.log(LogLevel.PARSER, "After process Parenthesis:",
		// root.toString());

		root = aggregateStrings(root.getIterator());

		// Log.log(LogLevel.PARSER, "After aggregateStrings:", root.toString());

		root = unfoldStrings(root.getIterator());

		// Log.log(LogLevel.PARSER, "After unfoldStrings:", root.toString());

		root = FunctionActivator.activateFunctions(root.getIterator());

		// Log.log(LogLevel.PARSER, "After activateFunctions:",
		// root.toString());

		TokenIterator iterator = root.tgetIterator();

		iterator = OperationActivator.activateOperations(iterator);

		// Log.log(LogLevel.PARSER, "After activateOperations:",
		// iterator.toString());

		Expression out = iterator.toExpression();

		if (out == null)
			throw new ParserException();

		final IIterator<DividedNode> dit = divided.getIterator();

		while (dit.hasNext()) {
			final DividedNode node = dit.next();
			final Expression expr = node.getValue();
			out = out.partialSubstitute(node.getLetter(), expr);
		}

		Log.log(LogLevel.PARSER, "After toExpression:", out.toString());

		return out;
	}

	private static TokenList tokenize(final String string) {
		final TokenList root = new TokenList();
		final char[] arr = string.toCharArray();
		for (int c = 0; c < arr.length; c++) {
			final char current = arr[c];
			if (current != ' ')
				root.add(new CharToken(current));
		}
		return root;
	}

	private static TokenList unfoldStrings(final IIterator<IToken> iterator) {
		final TokenList toret = new TokenList();
		while (iterator.hasNext()) {
			final IToken current = iterator.next();
			if (current instanceof TokenList)
				toret.add(unfoldStrings(((TokenList) current).getIterator()));
			else if (current instanceof CharList)
				toret.addRange(((CharList) current).getIterator());
			else
				toret.add(current);
		}
		return toret;
	}

	private static TokenList aggregateStrings(final IIterator<IToken> iterator) {
		final TokenList toret = new TokenList();
		while (iterator.hasNext()) {
			IToken token = iterator.next();
			if (token instanceof TokenList)
				token = aggregateStrings(((AbstractTokenList) token).getIterator());
			if (token instanceof CharToken) {
				final CharList charList = new CharList();
				while (token instanceof CharToken) {
					charList.add(token);
					if (iterator.hasNext())
						token = iterator.next();
					else {
						toret.addRange(charList.aggregate());
						return toret;
					}
				}
				toret.addRange(charList.aggregate());
				if (token instanceof TokenList)
					token = aggregateStrings(((AbstractTokenList) token).getIterator());
			}
			toret.add(token);
		}
		return toret;
	}

	private static ITokenList processParenthesis(final IIterator<IToken> iterator) throws ParserException {
		final ITokenList toret = new TokenList();
		while (iterator.hasNext()) {
			final IToken curr = iterator.next();
			if (curr instanceof ParToken) {
				final ParToken partoken = (ParToken) curr;
				if (partoken.isOpen()) {
					if (!iterator.hasNext())
						throw new ParserException();
					final ITokenList temp = processParenthesis(iterator);
					toret.add(temp);
				} else
					break;
			} else
				toret.add(curr);
		}
		return toret;
	}

	private static ITokenList aggregateNumbers(final IIterator<IToken> iterator, final boolean exact)
			throws ParserException {
		final ITokenList root = new TokenList();
		while (iterator.hasNext()) {
			IToken curr = iterator.next();
			char currChar = curr.toString().charAt(0);
			if (isDigit(currChar)) {
				final ITokenList temp = new TokenList();
				boolean appendIt = false;
				boolean isDouble = false;
				while (isDigit(currChar)) {
					if (currChar == '.' || currChar == '@')
						isDouble = true;
					temp.add(curr);
					appendIt = false;
					if (iterator.hasNext()) {
						curr = iterator.next();
						currChar = curr.toString().charAt(0);
						appendIt = true;
					} else
						break;
				}
				if (isDouble)
					if (exact)
						root.add(new PerfectDoubleToken(temp));
					else
						root.add(new DoubleToken(temp));
				else
					root.add(new IntToken(temp));
				if (appendIt)
					root.add(curr);
			} else
				root.add(curr);
		}
		return root;
	}

	private static boolean isDigit(final char currChar) {
		return currChar >= '0' && currChar <= '9' || currChar == '.' || currChar == '@';
	}

	private static ITokenList activateSyntax(final IIterator<IToken> iterator) {
		final ITokenList toret = new TokenList();
		// third pass: activate operators and parenthesis
		while (iterator.hasNext()) {
			final IToken curr = iterator.next();
			if (curr instanceof CharToken) {
				final char val = curr.toString().charAt(0);
				boolean found = false;
				for (int c = 0; c < Operation.OPERATIONS.length; c++)
					if (val == Operation.OPERATIONS[c]) {
						final OperationToken optok = new OperationToken(val);
						toret.add(optok);
						found = true;
						break;
					}
				if (!found)
					switch (val) {
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
			} else
				toret.add(curr);
		}
		return toret;
	}

	private static void activateMinus(final ITokenList toret) {
		final IToken last = toret.getLast();
		if (last instanceof OperationToken) {
			final char val = ((OperationToken) last).getVal();
			if (val == Operation.DIVISION || val == Operation.MULTIPLICATION) {
				toret.add(new FunctionToken("M", new Token[0]));
				return;
			}
		}
		toret.add(new OperationToken(Operation.ADDITION));
		toret.add(new IntToken(-1));
		toret.add(new OperationToken(Operation.MULTIPLICATION));
	}

	public static Expression parseOrDefault(final String test, final Expression zero) {
		try {
			return parse(test);
		} catch (final ParserException e) {
			Log.log(LogLevel.WARNING, "Parse error: " + e.getMessage() + " | " + e.toString());
			return zero;
		}
	}

	public static Expression parseOrDefault(final String string) {
		return parseOrDefault(string, Int.ZERO);
	}

	public static UserFunction parseUserFunction(final String string) throws ParserException {
		final int semicolon = string.indexOf(':');
		final int opened = string.indexOf('(');
		if (opened < 0 || semicolon < 0)
			throw new ParserException("':' or '(' missing in function definition");
		final String name = string.substring(0, opened);
		final String argString = string.substring(opened + 1, semicolon);
		final int num = argString.length() / 2;
		final char[] vars = new char[num];
		for (int c = 0; c < num; c++)
			vars[c] = argString.charAt(c * 2);
		final String rest = string.substring(semicolon + 2);
		final Expression expr = parseOrDefault(rest);
		return new UserFunction(num, name, vars, expr);
	}
}
