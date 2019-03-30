package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.IOutputtable;
import meplot.expressions.ISimplifiable;
import meplot.expressions.ISubstitutible;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunction;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;
import platform.log.Log;
import platform.log.LogLevel;

public final class Multiplication extends AbstractExpression implements IMultiplication {
	private final IExpressionList factors;

	public boolean contains(Expression expr, int start) {
		return factors.contains(expr, start);
	}

	public Multiplication(final Expression left, final Expression right) {
		factors = new ExpressionList(left, right);
	}

	private Multiplication(final IExpressionList left, final IExpressionList right) {
		factors = new ExpressionList(left, right);
	}

	public Multiplication(final IExpressionList val) {
		factors = new ExpressionList(val);
	}

	public Multiplication(final IIterator<Expression> iterator) {
		factors = new ExpressionList(iterator);
	}

	private Multiplication(final IExpressionList left, final Expression right) {
		factors = new ExpressionList(left);
		factors.add(right);
	}

	public Multiplication() {
		factors = new ExpressionList();
	}

	public Multiplication(final Expression val) {
		factors = new ExpressionList(val);
	}

	public Expression multiply(final Expression arg) {
		if (arg.isOne())
			return this;
		if (isOne())
			return arg;
		if (equals(arg))
			return new Power(this, Int.TWO);
		if (arg instanceof Multiplication)
			return new Multiplication(factors, ((Multiplication) arg).factors);
		return new Multiplication(factors, arg);
	}

	public Expression expand() {
		final IExpressionList temp = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof Sum) {
				temp.addRange(iterator);
				final ICalculable mul = new Multiplication(temp);
				final IIterator<Expression> add = ((Sum) curr).getIterator();
				final IExpressionList toret = new ExpressionList();
				while (add.hasNext())
					toret.add(mul.multiply(add.next()));
				return new Sum(toret);
			} else if (curr instanceof Power) {
				temp.add(curr.expand());
				temp.addRange(iterator);
				return new Multiplication(temp);
			} else
				temp.add(curr);
		}
		return this;
	}

	public boolean toStringStartsWith(char prefix) {
		final IIterator<Expression> iterator = getIterator();
		if (!iterator.hasNext())
			return false;
		if (factors.isSingle())
			return factors.getFirst().toStringStartsWith(prefix);
		if (isMinusOneOdd())
			return prefix == '-';
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (!isMinusOne(curr)) {
				if (curr instanceof Letter || curr instanceof IFunction || curr instanceof Power
						|| curr instanceof Matrix)
					return curr.toStringStartsWith(prefix);
				if (curr instanceof INumber)
					return curr.toStringStartsWith(prefix);
				return prefix == '(';
			}
		}
		return prefix == '1';
	}

	public void toString(final StringBuffer buffer) {
		final IIterator<Expression> iterator = getIterator();
		if (!iterator.hasNext())
			return;
		if (factors.isSingle()) {
			factors.getFirst().toString(buffer);
			return;
		}
		if (isMinusOneOdd()) {
			buffer.append('-');
			if (!iterator.hasNext()) {
				buffer.append('1');
				return;
			}
		}
		Expression last = null;
		boolean hasAppended = false;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr == null) {
				Log.log(LogLevel.ERROR, "WTHNULL in Multiplication.toString");
				continue;
			}
			if (isMinusOne(curr))
				continue;
			if (curr instanceof Letter || curr instanceof IFunction || curr instanceof Power
					|| curr instanceof Matrix) {
				curr.toString(buffer);
				hasAppended = true;
			} else if (curr instanceof INumber) {
				if (last != null)
					buffer.append('*');
				curr.toString(buffer);
				hasAppended = true;
			} else {
				buffer.append('(');
				curr.toString(buffer);
				buffer.append(')');
				hasAppended = true;
			}
			last = curr;
		}
		if (!hasAppended)
			buffer.append('1');
	}

	private boolean isMinusOneOdd() {
		boolean odd = false;
		for (Expression curr : this) {
			if (curr == null)
				continue;
			if (isMinusOne(curr))
				odd ^= true;
		}
		return odd;
	}

	private static boolean isMinusOne(final Expression curr) {
		return curr instanceof IInt && ((IInt) curr).getValue() == -1;
	}

	public boolean isZero() {
		if (factors.isEmpty())
			return false;
		if (factors.isSingle()) {
			final ICalculable value = factors.getFirst();
			if (value == null) {
				factors.isSingle();
				Log.log(new NullPointerException("Multiplication.isZero"));
				return false;
			}
			return value.isZero();
		}
		return false;
	}

	public boolean isOne() {
		if (factors.isEmpty())
			return true;
		if (factors.isSingle())
			return factors.getFirst().isOne();
		return false;
	}

	public Expression innerSimplify() {
		if (factors.isSingle())
			return factors.getFirst().partialSimplify();

		IIterator<Expression> iterator = getIterator();

		final Expression after = simplifyFactors(iterator);
		if (after.isZero())
			return Int.ZERO;

		if (after instanceof Multiplication) {
			final Multiplication afterM = (Multiplication) after;
			iterator = afterM.getIterator();

			if (!iterator.hasNext())
				return Int.ONE;

			while (iterator.hasNext()) {
				final Expression inner = innerMultiply(afterM, iterator);
				if (inner != null)
					return order(inner);
			}

			return afterM.order();
		}
		return after;
	}

	public Expression innerStepSimplify() {
		if (factors.isSingle())
			return factors.getFirst().innerStepSimplify();
		IIterator<Expression> iterator = getIterator();

		final Expression after = stepSimplifyFactors(iterator);
		if (after.isZero())
			return Int.ZERO;

		if (after instanceof Multiplication) {
			final Multiplication afterM = (Multiplication) after;
			if (!afterM.factors.equals(factors))
				return afterM.order();

			iterator = afterM.getIterator();

			while (iterator.hasNext()) {
				final Expression inner = innerMultiply(afterM, iterator);
				if (inner != null)
					return order(inner);
			}

			return afterM.order();
		}
		return after;
	}

	private static Expression simplifyFactors(final IIterator<Expression> iterator) {
		final IExpressionList after = new ExpressionList();
		while (iterator.hasNext()) {
			final ISimplifiable curr = iterator.next();
			final Expression currs = curr.partialSimplify();
			if (currs.isZero())
				return Int.ZERO;
			if (currs.isOne())
				continue;
			if (currs instanceof Multiplication) {
				final Expression expr = currs.partialSimplify();
				if (expr instanceof Multiplication) {
					final Multiplication clp = (Multiplication) expr;
					after.addRange(clp.factors);
				} else
					after.add(expr);
			} else
				after.add(currs);
		}
		return new Multiplication(after);
	}

	private static Expression stepSimplifyFactors(final IIterator<Expression> iterator) {
		final Multiplication after = new Multiplication();
		while (iterator.hasNext()) {
			final ISimplifiable curr = iterator.next();
			final Expression currs = curr.innerStepSimplify();
			/*
			 * if(currs.isZero()) return Int.ZERO; if(currs.isOne()) continue;
			 */
			if (currs instanceof Multiplication) {
				final Expression expr = currs.innerStepSimplify();
				if (expr instanceof Multiplication) {
					final Multiplication clp = (Multiplication) expr;
					after.factors.addRange(clp.factors);
				} else
					after.factors.add(expr);
			} else
				after.factors.add(currs);
		}
		return after;
	}

	private static Expression innerMultiply(final Multiplication after, final IIterator<Expression> iterator) {
		final Expression first = iterator.next();
		final IIterator<Expression> sub = iterator.subIterator();
		while (sub.hasNext()) {
			final Expression second = sub.next();
			if (first.compatible(second, Operation.MULTIPLICATION)) {
				final Expression toret = first.multiply(second);
				return finishInner(after, first, second, toret);
			}
			if (second.compatible(first, Operation.MULTIPLICATION)) {
				final Expression toret = second.multiply(first);
				return finishInner(after, first, second, toret);
			}
		}
		return null;
	}

	private static Expression finishInner(final Multiplication after, final Expression left, final Expression right,
			final Expression initial) {
		final IIterator<Expression> iterator = after.getIterator();
		Expression toret = initial;
		boolean jumpedLeft = false;
		boolean jumpedRight = false;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if ((jumpedLeft || curr != left) && (jumpedRight || curr != right))
				toret = toret.multiply(curr);
			else if (!jumpedLeft && curr == left)
				jumpedLeft = true;
			else if (curr == right)
				jumpedRight = true;
		}
		return toret;
	}

	private static Expression order(final Expression toret) {
		if (!(toret instanceof Multiplication))
			return toret;
		return ((Multiplication) toret).order();
	}

	private Multiplication order() {
		final IExpressionList toret = new ExpressionList();
		IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof INumber)
				toret.add(curr);
		}
		for (char t = 'a'; t <= 'z'; t++) {
			iterator = getIterator();
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				if (curr instanceof Letter && ((Letter) curr).getLetter() == t)
					toret.add(curr);
			}
		}
		iterator = getIterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (!(curr instanceof INumber || curr instanceof Letter))
				toret.add(curr);
		}
		return new Multiplication(toret);
	}

	public boolean compatible(final Expression elementAt, final char operation) {
		if (elementAt instanceof Multiplication && operation == Operation.MULTIPLICATION)
			return true;
		if (operation == Operation.DIVISION)
			return isDivisible(elementAt);
		if (operation == Operation.ADDITION)
			if (nCompatible(elementAt))
				return true;
		if (operation == Operation.MULTIPLICATION)
			return true;
		return super.compatible(elementAt, operation);
	}

	// return whether the only difference is a INumber
	private boolean nCompatible(final Expression arg) {
		if (arg instanceof Multiplication) {
			final Multiplication marg = (Multiplication) arg;
			return nCheckContains(factors, marg.factors) && nCheckContains(marg.factors, factors);
		}
		final IIterator<Expression> iterator = factors.getIterator();
		boolean usd = false;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof INumber)
				continue;
			if (usd || !curr.equals(arg))
				return false;
			usd = true;
		}
		return usd;
	}

	private static boolean nCheckContains(final IExpressionList small, final IExpressionList big) {
		final IIterator<Expression> smallIt = small.getIterator();
		final boolean[] usd = new boolean[big.length()];
		while (smallIt.hasNext()) {
			final Expression curr = smallIt.next();
			if (curr instanceof INumber)
				continue;
			final IIterator<Expression> bigIt = big.getIterator();
			if (!nCheckContainsSearch(usd, curr, bigIt))
				return false;
		}
		return true;
	}

	private static boolean nCheckContainsSearch(final boolean[] usd, final Expression curr,
			final IIterator<Expression> bigIt) {
		for (int i = 0; bigIt.hasNext(); i++) {
			final Expression test = bigIt.next();
			if (!usd[i] && test.equals(curr)) {
				usd[i] = true;
				return true;
			}
		}
		return false;
	}

	public Expression add(final Expression arg) {
		if (factors.isSingle())
			return factors.getFirst().add(arg);

		final Multiplication marg;
		if (arg instanceof Multiplication)
			marg = (Multiplication) arg;
		else
			marg = new Multiplication(arg);

		if (!nCompatible(marg)) {
			if (factors.contains(arg))
				return new Sum(this, marg);
			return super.add(arg);
		}
		final IExpressionList thisnonINumbers = extractNonINumbers(factors);
		final IExpressionList argnonINumbers = extractNonINumbers(marg.factors);
		if (!thisnonINumbers.equals(argnonINumbers))
			return super.add(arg);
		final INumber thiscoeff = getCoefficent(factors);
		final INumber argcoeff = getCoefficent(marg.factors);
		final INumber coeff = thiscoeff.add(argcoeff);
		return coeff.multiply(new Multiplication(thisnonINumbers));
	}

	public INumber coefficent() {
		return getCoefficent(factors);
	}

	private static INumber getCoefficent(final IExpressionList list) {
		INumber toret = Int.ONE;
		final IIterator<Expression> iterator = list.getIterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof INumber)
				toret = toret.multiply((INumber) curr);
		}
		return toret;
	}

	private static IExpressionList extractNonINumbers(final IExpressionList list) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = list.getIterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (!(curr instanceof INumber))
				toret.add(curr);
		}
		return toret;
	}

	private boolean isDivisible(final Expression elementAt) {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ICalculable curr = iterator.next();
			if (curr.compatible(elementAt, Operation.DIVISION))
				return true;
		}
		return false;
	}

	public Expression divide(final Expression arg) {
		if (compatible(arg, Operation.DIVISION)) {
			final IExpressionList toret = new ExpressionList();
			final IIterator<Expression> iterator = getIterator();
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				if (curr.compatible(arg, Operation.DIVISION)) {
					toret.add(curr.divide(arg));
					toret.addRange(iterator);
				} else
					toret.add(curr);
			}
			return new Multiplication(toret);
		}
		return super.divide(arg);
	}

	public double dvalue(final char letter, final double value) {
		INumber toret = Int.ONE;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret = toret.multiply(iterator.next().value(letter, value));
		return toret.dvalue();
	}

	public INumber value(final IValueList list) {
		INumber toret = Int.ONE;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret = toret.multiply(iterator.next().value(list));
		return toret;
	}

	public Expression partialSubstitute(final IValueList valueList) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ISubstitutible curr = iterator.next();
			final Expression par = curr.partialSubstitute(valueList);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public Expression partialSubstitute(final char var, final double value) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ISubstitutible curr = iterator.next();
			final Expression par = curr.partialSubstitute(var, value);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public Expression partialSubstitute(final char var, final Expression value) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ISubstitutible curr = iterator.next();
			final Expression par = curr.partialSubstitute(var, value);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public IIterator<Expression> getIterator() {
		return factors.getIterator();
	}

	public boolean needParenthesis() {
		return !factors.isSingle();
	}

	public boolean hasLetter(final char arg) {
		return factors.hasLetter(arg);
	}

	public boolean equals(final Object obj) {
		if (!(obj instanceof Multiplication))
			return super.equals(obj);
		final Multiplication mul = (Multiplication) obj;
		return nCompatible(mul) && coefficent().equals(mul.coefficent());
	}

	public boolean containsMatrix() {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			if (iterator.next().containsMatrix())
				return true;
		return false;
	}

	public void toFullString(final StringBuffer buffer) {
		buffer.append("*(");
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final IOutputtable curr = iterator.next();
			if (curr == null) {
				Log.log(LogLevel.ERROR, "WTHNULL in Multiplication.toFullString");
				continue;
			}
			curr.toFullString(buffer);
			if (iterator.hasNext())
				buffer.append(',');
		}
		buffer.append(')');
	}

	public ITensor matrixDvalue(final IValueList valueList) {
		ITensor toret = Int.ONE;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret = toret.multiply(iterator.next().partialSubstitute(valueList).matrixDvalue(valueList));
		return toret;
	}

	public ITensor matrixDvalue(final char letter, final double value) {
		ITensor toret = Int.ONE;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret = toret.multiply(iterator.next().partialSubstitute(letter, value).matrixDvalue(letter, value));
		return toret;
	}

	public double fdvalue(final char letter, final double value) {
		double toret = 1;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret *= iterator.next().fdvalue(letter, value);
		return toret;
	}

	public boolean isFullDouble() {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			if (!iterator.next().isFullDouble())
				return false;
		return true;
	}

	public boolean isIdentical(final Expression other) {
		if (!(other instanceof Multiplication))
			return false;
		final Multiplication oth = (Multiplication) other;
		return areIdentical(factors.getIterator(), oth.factors.getIterator());
	}

	public Expression accept(final IExpressionVisitor visitor) {
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer) {
		final IIterator<Expression> iterator = getIterator();
		if (!iterator.hasNext())
			return;
		if (factors.isSingle()) {
			factors.getFirst().toHtml(buffer);
			return;
		}
		if (isMinusOneOdd()) {
			buffer.append('-');
			if (!iterator.hasNext()) {
				buffer.append('1');
				return;
			}
		}
		Expression last = null;
		boolean hasAppended = false;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr == null) {
				Log.log(LogLevel.ERROR, "WTHNULL in Multiplication.toString");
				continue;
			}
			if (isMinusOne(curr))
				continue;
			if (curr instanceof Letter || curr instanceof IFunction || curr instanceof Power
					|| curr instanceof Matrix) {
				curr.toWrappedHtml(buffer);
				hasAppended = true;
			} else if (curr instanceof INumber) {
				if (last != null)
					buffer.append('*');
				curr.toWrappedHtml(buffer);
				hasAppended = true;
			} else {
				buffer.append('(');
				curr.toWrappedHtml(buffer);
				buffer.append(')');
				hasAppended = true;
			}
			last = curr;
		}
		if (!hasAppended)
			buffer.append('1');
	}
}
