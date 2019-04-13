package meplot.parser.utils;

import meplot.parser.ParserException;
import meplot.parser.tokens.AbstractTokenList;
import meplot.parser.tokens.FunctionToken;
import meplot.parser.tokens.IToken;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.TokenList;
import org.jetbrains.annotations.NotNull;
import platform.lists.IterableExtensions;

import java.util.Iterator;

public final class FunctionActivator {
	private static final String COMMA = ",";

	private FunctionActivator() {
	}

	public static ITokenList activateFunctions(final Iterable<IToken> iterable) throws ParserException {
		final ITokenList toret = new TokenList();
		Iterator<IToken> iterator = iterable.iterator();
		while (iterator.hasNext())
			toret.add(activateFunctionsToken(iterator.next(), iterator));
		return toret;
	}

	private static IToken activateFunctionsToken(final IToken token, final Iterator<IToken> iterator)
			throws ParserException {
		if (token instanceof AbstractTokenList)
			return activateFunctions((AbstractTokenList) token);

		if (token instanceof FunctionToken)
			return activateFunctions((FunctionToken) token, iterator);

		return token;
	}

	private static IToken activateFunctions(final FunctionToken funTok, final Iterator<IToken> iterator)
			throws ParserException {
		if (!iterator.hasNext())
			throw new ParserException();
		IToken next = iterator.next();
		if (next instanceof AbstractTokenList || next instanceof FunctionToken)
			next = activateFunctionsToken(next, iterator);
		final int needs = funTok.needs();
		if (needs == 0)
			return funTok;

		if (needs > 0) {
			final IToken[] args = new IToken[needs];
			if (needs == 1)
				args[0] = next;
			else if (next instanceof AbstractTokenList) {
				final ITokenList asList = (ITokenList) next;
				final Iterator<IToken> inner = asList.iterator();
				for (int i = 0; i < needs; i++)
					args[i] = getUntilComma(inner);
			} else {
				final TokenList zero = new TokenList(next);
				final IToken toadd = activateFunctionsToken(getUntilComma(iterator), iterator);
				if (toadd instanceof AbstractTokenList)
					zero.addRange((AbstractTokenList) toadd);
				else
					zero.add(toadd);
				args[0] = zero;
				for (int i = 1; i < needs - 1; i++)
					args[i] = activateFunctionsToken(getUntilComma(iterator), iterator);
				if (!iterator.hasNext())
					throw new ParserException();
				args[needs - 1] = activateFunctionsToken(iterator.next(), iterator);
			}
			return funTok.fill(args);
		}
		final TokenList args = new TokenList();
		if (next instanceof ITokenList) {
			final ITokenList asList = (ITokenList) next;
			final Iterator<IToken> inner = asList.iterator();
			while (true) {
				final ITokenList arg = getUntilComma(inner);
				if (IterableExtensions.isEmpty(arg))
					break;
				args.add(arg);
			}
		} else {
			final TokenList zero = new TokenList(next);
			final IToken toadd = activateFunctionsToken(getUntilComma(iterator), iterator);
			if (toadd instanceof ITokenList)
				zero.addRange((ITokenList) toadd);
			else
				zero.add(toadd);
			args.add(zero);
			while (true) {
				final ITokenList arg = getUntilComma(iterator);
				if (IterableExtensions.isEmpty(arg))
					break;
				args.add(activateFunctionsToken(arg, iterator));
			}
			if (iterator.hasNext())
				args.add(activateFunctionsToken(iterator.next(), iterator));
		}
		return funTok.fill(args);
	}

	@NotNull
	private static TokenList getUntilComma(Iterator<IToken> inner) {
		return new TokenList(IterableExtensions.until(inner, COMMA));
	}
}
