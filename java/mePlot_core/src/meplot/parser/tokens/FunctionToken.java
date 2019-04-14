package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunctor;
import meplot.expressions.functions.UserFunction;
import meplot.expressions.functions.algebra.Span;
import meplot.expressions.functions.complex.Arg;
import meplot.expressions.functions.complex.Im;
import meplot.expressions.functions.complex.Re;
import meplot.expressions.functions.exp.*;
import meplot.expressions.functions.matrix.*;
import meplot.expressions.functions.ode.GenericOde;
import meplot.expressions.functions.ode.Ode;
import meplot.expressions.functions.operations.Derivative;
import meplot.expressions.functions.operations.Integral;
import meplot.expressions.functions.operations.Mod;
import meplot.expressions.functions.other.*;
import meplot.expressions.functions.piecewise.*;
import meplot.expressions.functions.trig.*;
import meplot.expressions.numbers.Int;
import meplot.parser.ParserException;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.List;

public final class FunctionToken extends Token {
	/**
	 * Magic function: M(x) means -x.
	 */
	private static final String MAGIC_FUNCTION = "M";
	private final String val;
	private final IList<IToken> args;

	public FunctionToken(final String name, final IList<IToken> tokens) {
		val = name;
		args = tokens;
	}

	// Functions requiring special attention
	private static final String[] OTHERS = new String[] { MAGIC_FUNCTION };

	// Empty array, avoids multiple creations
	private static final Expression[] NARR = new Expression[0];

	// Add here new functions
	private static final IFunctor[] DEFAULT_FUNCTIONS = new IFunctor[] { new Sinh(Int.ZERO), new Cosh(Int.ZERO),
			new Asin(Int.ZERO), new Sin(Int.ZERO), new Cos(Int.ZERO), new Atan(Int.ZERO), new Tan(Int.ZERO),
			new Abs(Int.ZERO), new Floor(Int.ZERO), new Ln(Int.ZERO), new Exp(Int.ZERO), new Sqrt(Int.ZERO),
			new Arg(Int.ZERO), new Re(Int.ZERO), new Im(Int.ZERO), new Sign(Int.ZERO), new Log10(Int.ZERO),
			new Det(Int.ZERO), new Transpose(Int.ZERO), new Rank(Int.ZERO), new Len(Int.ZERO),
			new Hold(Int.ZERO), new Cbrt(Int.ZERO), new Gradient(Int.ZERO),
			new MaxNorm(NARR), new PNorm(NARR), new Piecewise(NARR), new Max(NARR),
			new Min(NARR), new Mandelbrot(NARR), new Mod(NARR), new Derivative(NARR), new Integral(NARR),
			new Ackermann(NARR), new Span(NARR), new GenericOde(NARR), new Ode(NARR), new Gcd(NARR) };

	private static List<UserFunction> userFunctions = new List<>();

	public Expression toExpression() throws ParserException {
		final Expression[] eargs = getArgs();

		for (IFunctor defaultFunction : DEFAULT_FUNCTIONS)
			if (defaultFunction.getName().equals(val))
				return defaultFunction.gfill(eargs);

		for (UserFunction uff : userFunctions)
			if (uff.getName().equals(val))
				return uff.fill(eargs);
		if (MAGIC_FUNCTION.equals(val))
			return eargs[0].opposite();

		throw new ParserException("toExpression of unknown function");
	}

	private Expression[] getArgs() throws ParserException {
		if (args == null) {
			return new Expression[0];
		}

		final IList<Expression> eargs = new List<>();
		for (IToken arg : args)
			eargs.add(arg == null ? Letter.I : arg.toExpression());
		return IterableExtensions.toArray(Expression.class, eargs);
	}

	public String toString() {
		final StringBuilder toret = new StringBuilder();
		toString(toret);
		return toret.toString();
	}

	public void toString(final StringBuilder toret) {
		toret.append(val);
		toret.append('(');
		if (args == null)
			toret.append("?");
		else {
			boolean first = true;
			for (IToken arg : args) {
				if (!first)
					toret.append(',');
				first = false;
				toret.append(stringize(arg));
			}
		}
		toret.append(')');
	}

	private static String stringize(final IToken args2) {
		return args2 == null ? "?" : args2.toString();
	}

	public IToken[] getValues() {
		return IterableExtensions.toArray(IToken.class, args);
	}

	public static ITokenList parse(final String input) {
		final TokenList toret = new TokenList();
		final int start = firstF(input);
		if (start >= 0) {
			final int end = firstFe(input);
			if (start > 0) {
				final ITokenList tokenList = parse(input.substring(0, start));
				toret.addRange(tokenList);
			}
			final String substring = input.substring(start, end);
			final FunctionToken token = new FunctionToken(substring, new List<>());
			toret.add(token);
			if (input.length() > end) {
				final String remains = input.substring(end);
				final ITokenList parsedRemains = parse(remains);
				toret.addRange(parsedRemains);
			}
		} else
			toret.add(new CharList(input));
		return toret;
	}

	private static int firstFe(final String name) {
		for (IFunctor defaultFunction : DEFAULT_FUNCTIONS) {
			final int index = name.indexOf(defaultFunction.getName());
			if (index >= 0)
				return index + defaultFunction.getName().length();
		}

		for (String other : OTHERS) {
			final int index = name.indexOf(other);
			if (index >= 0)
				return index + other.length();
		}

		for (UserFunction uff : userFunctions) {
			final int index = name.indexOf(uff.getName());
			if (index >= 0)
				return index + uff.getName().length();
		}

		return -1;
	}

	private static int firstF(final String name) {
		for (IFunctor defaultFunction : DEFAULT_FUNCTIONS) {
			final int index = name.indexOf(defaultFunction.getName());
			if (index >= 0)
				return index;
		}

		for (String other : OTHERS) {
			final int index = name.indexOf(other);
			if (index >= 0)
				return index;
		}

		for (UserFunction uff : userFunctions) {
			final int index = name.indexOf(uff.getName());
			if (index >= 0)
				return index;
		}

		return -1;
	}

	public Token fill(final IList<IToken> args)
	{
		return new FunctionToken(val, args);
	}

	public int needs() {
		try {
			return match().needs();
		} catch (final ParserException e) {
			return -1;
		}
	}

	private IFunctor match() throws ParserException {
		return match(val);
	}

	public static IFunctor match(final String value) throws ParserException {
		for (IFunctor defaultFunction : DEFAULT_FUNCTIONS)
			if (defaultFunction.getName().equals(value))
				return defaultFunction;

		for (UserFunction uff : userFunctions)
			if (uff.getName().equals(value))
				return uff;

		if (MAGIC_FUNCTION.equals(value))
			return new FakeFunction(1);

		throw new ParserException("Match of unknown function");
	}

	public static void setUserFunctions(final List<UserFunction> toadd) {
		userFunctions = new List<>();
		userFunctions.addRange(toadd);
	}

}
