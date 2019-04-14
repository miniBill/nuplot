package meplot.expressions.operations;

import meplot.expressions.*;
import meplot.expressions.functions.IFunction;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;
import platform.lists.IterableExtensions;
import platform.lists.List;
import platform.log.Log;
import platform.log.LogLevel;

import java.util.Iterator;

public final class Multiplication extends AbstractExpression implements IMultiplication {
	private final List<Expression> factors;

	public Multiplication(final Expression left, final Expression right) {
		factors = new ExpressionList(left, right);
	}

	private Multiplication(final Iterable<Expression> left, final Iterable<Expression> right) {
		factors = new List<>(left);
		factors.addRange(right);
	}

	public Multiplication(final Iterable<Expression> val) {
		factors = new List<>(val);
	}

	private Multiplication(final Iterable<Expression> left, final Expression right) {
		factors = new List<>(left);
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
		final Iterator<Expression> iterator = iterator();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof Sum) {
				temp.addRange(iterator);
				final ICalculable mul = new Multiplication(temp);
				final IExpressionList toret = new ExpressionList();
				for (Expression expression : (Sum) curr)
					toret.add(mul.multiply(expression));
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
		if (IterableExtensions.isSingle(factors))
			return factors.getFirst().toStringStartsWith(prefix);
		if (isMinusOneOdd())
			return prefix == '-';
		for (Expression curr : this) {
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

	public void toString(final StringBuilder buffer) {
		final Iterator<Expression> iterator = iterator();
		if (!iterator.hasNext())
			return;
		if (IterableExtensions.isSingle(factors)) {
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
		for (Expression curr : IterableExtensions.wrap(iterator)) {
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
		for (Expression curr : this)
			if (curr != null && isMinusOne(curr))
				odd ^= true;
		return odd;
	}

	private static boolean isMinusOne(final Expression curr) {
		return curr instanceof IInt && ((IInt) curr).getValue() == -1;
	}

	public boolean isZero() {
		if (IterableExtensions.isEmpty(factors))
			return false;
		if (IterableExtensions.isSingle(factors)) {
			final ICalculable value = factors.getFirst();
			if (value == null) {
				IterableExtensions.isSingle(factors);
				Log.log(new NullPointerException("Multiplication.isZero"));
				return false;
			}
			return value.isZero();
		}
		return false;
	}

	public boolean isOne() {
		if (IterableExtensions.isEmpty(factors))
			return true;
		if (IterableExtensions.isSingle(factors))
			return factors.getFirst().isOne();
		return false;
	}

	public Expression innerSimplify() {
		if (IterableExtensions.isSingle(factors))
			return factors.getFirst().partialSimplify();

		final Expression after = simplifyFactors(this);
		if (after.isZero())
			return Int.ZERO;

		if (after instanceof Multiplication) {
			final Multiplication afterM = (Multiplication) after;

			if (IterableExtensions.isEmpty(afterM))
				return Int.ONE;

			Iterator<Expression> iterator2 = afterM.iterator();
			while (iterator2.hasNext()) {
				final Expression inner = innerMultiply(afterM, iterator2);
				if (inner != null)
					return order(inner);
			}

			return afterM.order();
		}
		return after;
	}

	public Expression innerStepSimplify() {
		if (IterableExtensions.isSingle(factors))
			return factors.getFirst().innerStepSimplify();

		final Expression after = stepSimplifyFactors(this);
		if (after.isZero())
			return Int.ZERO;

		if (after instanceof Multiplication) {
			final Multiplication afterM = (Multiplication) after;
			if (!afterM.factors.equals(factors))
				return afterM.order();

			Iterator<Expression> iterator = afterM.iterator();

			while (iterator.hasNext()) {
				final Expression inner = innerMultiply(afterM, iterator);
				if (inner != null)
					return order(inner);
			}

			return afterM.order();
		}
		return after;
	}

	private static Expression simplifyFactors(final Iterable<Expression> iterable) {
		final List<Expression> after = new List<>();
		for (ISimplifiable curr : iterable) {
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

	private static Expression stepSimplifyFactors(final Iterable<Expression> iterable) {
		final Multiplication after = new Multiplication();
		for (ISimplifiable curr : iterable) {
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

	private static Expression innerMultiply(final Multiplication after, final Iterator<Expression> iterator) {
		final Expression first = iterator.next();
		for (Expression second : IterableExtensions.clone(iterator)) {
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
		Expression toret = initial;
		boolean jumpedLeft = false;
		boolean jumpedRight = false;
		for (Expression curr : after) {
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
		for (Expression curr : this)
			if (curr instanceof INumber)
				toret.add(curr);
		for (char t = 'a'; t <= 'z'; t++)
			for (Expression curr : this)
				if (curr instanceof Letter && ((Letter) curr).getLetter() == t)
					toret.add(curr);
		for (Expression curr : this)
			if (!(curr instanceof INumber || curr instanceof Letter))
				toret.add(curr);
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
		boolean usd = false;
		for (Expression curr : factors) {
			if (curr instanceof INumber)
				continue;
			if (usd || !curr.equals(arg))
				return false;
			usd = true;
		}
		return usd;
	}

	private static boolean nCheckContains(final Iterable<Expression> small, final Iterable<Expression> big) {
		final boolean[] used = new boolean[IterableExtensions.length(big)];
		for (Expression curr : small) {
			if (curr instanceof INumber)
				continue;

			int i = 0;
			boolean found = false;
			for (Expression test : big) {
				if (!used[i] && test.equals(curr)) {
					used[i] = true;
					found = true;
					break;
				}
				i++;
			}
			if (!found)
				return false;
		}
		return true;
	}

	public Expression add(final Expression arg) {
		if (IterableExtensions.isSingle(factors))
			return factors.getFirst().add(arg);

		final Multiplication marg;
		if (arg instanceof Multiplication)
			marg = (Multiplication) arg;
		else
			marg = new Multiplication(arg);

		if (!nCompatible(marg)) {
			if (IterableExtensions.contains(factors, arg))
				return new Sum(this, marg);
			return super.add(arg);
		}
		final List<Expression> thisnonINumbers = extractNonINumbers(factors);
		final List<Expression> argnonINumbers = extractNonINumbers(marg.factors);
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

	private static INumber getCoefficent(final Iterable<Expression> list) {
		INumber toret = Int.ONE;
		for (Expression curr : list)
			if (curr instanceof INumber)
				toret = toret.multiply((INumber) curr);
		return toret;
	}

	private static List<Expression> extractNonINumbers(final Iterable<Expression> list) {
		final List<Expression> toret = new List<>();
		for (Expression curr : list)
			if (!(curr instanceof INumber))
				toret.add(curr);
		return toret;
	}

	private boolean isDivisible(final Expression elementAt) {
		for (ICalculable curr : this)
			if (curr.compatible(elementAt, Operation.DIVISION))
				return true;
		return false;
	}

	public Expression divide(final Expression arg) {
		if (compatible(arg, Operation.DIVISION)) {
			final List<Expression> toret = new List<>();
			final Iterator<Expression> iterator = iterator();
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				if (curr.compatible(arg, Operation.DIVISION)) {
					toret.add(curr.divide(arg));
					IterableExtensions.addRange(toret, iterator);
				} else
					toret.add(curr);
			}
			return new Multiplication(toret);
		}
		return super.divide(arg);
	}

	public double dvalue(final char letter, final double value) {
		INumber toret = Int.ONE;
		for (Expression expression : this)
			toret = toret.multiply(expression.value(letter, value));
		return toret.dvalue();
	}

	public INumber value(final IValueList list) {
		INumber toret = Int.ONE;
		for (Expression expression : this)
			toret = toret.multiply(expression.value(list));
		return toret;
	}

	public Expression partialSubstitute(final IValueList valueList) {
		final IExpressionList toret = new ExpressionList();
		for (ISubstitutible curr : this) {
			final Expression par = curr.partialSubstitute(valueList);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public Expression partialSubstitute(final char var, final double value) {
		final IExpressionList toret = new ExpressionList();
		for (ISubstitutible curr : this) {
			final Expression par = curr.partialSubstitute(var, value);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public Expression partialSubstitute(final char var, final Expression value) {
		final IExpressionList toret = new ExpressionList();
		for (ISubstitutible curr : this) {
			final Expression par = curr.partialSubstitute(var, value);
			toret.add(par);
		}
		return new Multiplication(toret);
	}

	public Iterator<Expression> iterator() {
		return factors.iterator();
	}

	public boolean needParenthesis() {
		return !IterableExtensions.isSingle(factors);
	}

	public boolean hasLetter(final char arg) {
		return ExpressionList.hasLetter(factors, arg);
	}

	public boolean equals(final Object obj) {
		if (!(obj instanceof Multiplication))
			return super.equals(obj);
		final Multiplication mul = (Multiplication) obj;
		return nCompatible(mul) && coefficent().equals(mul.coefficent());
	}

	public boolean containsMatrix() {
		for (Expression expression : this)
			if (expression.containsMatrix())
				return true;
		return false;
	}

	public void toFullString(final StringBuilder buffer) {
		buffer.append("*(");
		boolean first = true;
		for (IOutputtable curr : this) {
			if (!first)
				buffer.append(',');
			first = false;
			if (curr == null) {
				Log.log(LogLevel.ERROR, "WTHNULL in Multiplication.toFullString");
				continue;
			}
			curr.toFullString(buffer);
		}
		buffer.append(')');
	}

	public ITensor matrixDvalue(final IValueList valueList) {
		ITensor toret = Int.ONE;
		for (Expression expression : this)
			toret = toret.multiply(expression.partialSubstitute(valueList).matrixDvalue(valueList));
		return toret;
	}

	public ITensor matrixDvalue(final char letter, final double value) {
		ITensor toret = Int.ONE;
		for (Expression expression : this)
			toret = toret.multiply(expression.partialSubstitute(letter, value).matrixDvalue(letter, value));
		return toret;
	}

	public double fdvalue(final char letter, final double value) {
		double toret = 1;
		for (Expression expression : this)
			toret *= expression.fdvalue(letter, value);
		return toret;
	}

	public boolean isFullDouble() {
		for (Expression expression : this)
			if (!expression.isFullDouble())
				return false;
		return true;
	}

	public boolean isIdentical(final Expression other) {
		if (!(other instanceof Multiplication))
			return false;
		final Multiplication oth = (Multiplication) other;
		return areIdentical(factors.iterator(), oth.factors.iterator());
	}

	public Expression accept(final IExpressionVisitor visitor) {
		return visitor.visit(this);
	}

	public void toHtml(final StringBuilder buffer) {
		if (IterableExtensions.isSingle(factors)) {
			factors.getFirst().toHtml(buffer);
			return;
		}
		if (isMinusOneOdd()) {
			final Iterator<Expression> iterator = iterator();
			buffer.append('-');
			if (!iterator.hasNext()) {
				buffer.append('1');
				return;
			}
		}
		Expression last = null;
		boolean hasAppended = false;
		for (Expression curr : this) {
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
