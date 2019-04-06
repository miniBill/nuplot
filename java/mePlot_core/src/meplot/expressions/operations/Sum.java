package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.ISimplifiable;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import platform.lists.IIterable;
import platform.lists.IIterator;

public class Sum extends AbstractExpression implements IIterable<Expression>, Expression {
	private static Expression finishInner(final IExpressionList after, final Expression first, final Expression second,
			final Expression initial) {
		final IIterator<Expression> iterator = after.getIterator();
		Expression toret = initial;
		boolean jumpedFirst = false;
		boolean jumpedSecond = false;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if ((jumpedFirst || curr != first) && (jumpedSecond || curr != second))
				toret = toret.add(curr);
			else if (!jumpedFirst && curr == first)
				jumpedFirst = true;
			else if (curr == second)
				jumpedSecond = true;
		}
		return toret;
	}

	private static Expression order(final Expression inner) {
		if (!(inner instanceof Sum))
			return inner;
		return ((Sum) inner).order();
	}

	private static Expression order(final IIterable<Expression> iiterable) {
		final IExpressionList rest = new ExpressionList();
		if (!iiterable.getFirst().toString().startsWith("-"))
			return null;
		Expression found = null;
		for (Expression curr : iiterable) {
			if (found != null || curr.toString().startsWith("-"))
				rest.add(curr);
			else {
				found = curr;
			}
		}
		if (found == null)
			return null;
		IExpressionList result = new ExpressionList(found);
		result.addRange(rest);
		return new Sum(result);
	}

	private static Expression trySquare(final Expression curr) {
		if (curr instanceof Power) {
			final Power pow = (Power) curr;
			if (pow.getExponent() instanceof Int) {
				final Int eint = (Int) pow.getExponent();
				if (eint.getValue() == 2)
					return pow.getBase();
			}
		}
		return null;
	}

	private final IExpressionList addends = new ExpressionList();

	public Sum(final Expression left) {
		if (left instanceof Sum)
			addends.addRange(((Sum) left).getIterator());
		else
			addends.add(left);
	}

	public Sum(final Expression left, final Expression right) {
		addends.add(left);
		addends.add(right);
	}

	public Sum(final IIterator<Expression> iterator) {
		addends.addRange(iterator);
	}

	private Sum(final IIterator<Expression> list1, final IIterator<Expression> list2) {
		addends.addRange(list1);
		addends.addRange(list2);
	}

	public Sum(final IExpressionList list) {
		addends.addRange(list.getIterator());
	}

	/**
	 * {@inheritDoc}
	 */
	public final Expression add(final Expression arg) {
		if (arg instanceof Sum)
			return add((Sum) arg);
		return super.add(arg);
	}

	private Sum add(final Sum arg) {
		return new Sum(getIterator(), arg.getIterator());
	}

	private boolean canDivide(final ICalculable base) {
		return equals(base) || equals(SimplificationHelper.simplify(base.opposite()));
	}

	public final boolean compatible(final Expression elementAt, final char operation) {
		if (elementAt instanceof Sum && (operation == Operation.ADDITION || operation == Operation.MULTIPLICATION))
			return true;
		if (operation == Operation.DIVISION && isDivisible(elementAt))
			return true;
		if (elementAt instanceof INumber && operation == Operation.MULTIPLICATION)
			return true;
		if (elementAt instanceof Power && (operation == Operation.MULTIPLICATION || operation == Operation.DIVISION)
				&& canDivide(((Power) elementAt).getBase()))
			return true;
		return super.compatible(elementAt, operation);
	}

	public final boolean containsMatrix() {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			if (iterator.next().containsMatrix())
				return true;
		return false;
	}

	public final Expression divide(final Expression arg) {
		if (arg instanceof Power) {
			final Power pow = (Power) arg;
			if (equals(pow.getBase()))
				return new Power(pow.getBase(), pow.getExponent().opposite().add(Int.ONE));
			if (equals(SimplificationHelper.simplify(pow.getBase().opposite())))
				return new Power(pow.getBase(), pow.getExponent().opposite().add(Int.ONE)).opposite();
		}
		if (isDivisible(arg)) {
			final IExpressionList toret = new ExpressionList();
			final IIterator<Expression> iterator = getIterator();
			while (iterator.hasNext()) {
				final ICalculable curr = iterator.next();
				final ISimplifiable div = curr.divide(arg);
				final Expression sim = div.partialSimplify();
				toret.add(sim);
			}
			return new Sum(toret);
		}
		return super.divide(arg);
	}

	public final double dvalue(final char letter, final double value) {
		double toret = 0;
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret += iterator.next().dvalue(letter, value);
		return toret;
	}

	public final boolean equals(final Object obj) {
		if (!(obj instanceof Sum))
			return super.equals(obj);
		final Sum sumobj = (Sum) obj;
		return addends.checkContains(sumobj.addends) && sumobj.addends.checkContains(addends);
	}

	/**
	 * {@inheritDoc}
	 */
	public final Expression expand() {
		boolean changed = false;
		final IIterator<Expression> iterator = getIterator();
		final IExpressionList toret = new ExpressionList();
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			final Expression exp = curr.expand();
			if (curr.equals(exp))
				toret.add(curr);
			else {
				changed = true;
				toret.add(exp);
			}
		}
		if (changed)
			return new Sum(toret);
		return this;
	}

	public final double fdvalue(final char letter, final double value) {
		final IIterator<Expression> iterator = getIterator();
		double toret = 0;
		while (iterator.hasNext())
			toret += iterator.next().fdvalue(letter, value);
		return toret;
	}

	public final IIterator<Expression> getIterator() {
		return addends.getIterator();
	}

	public final int hashCode() {
		return toString().hashCode() ^ addends.hashCode();
	}

	public final boolean hasLetter(final char var) {
		return addends.hasLetter(var);
	}

	public final Expression innerSimplify() {
		if (addends.isSingle())
			return addends.getFirst().partialSimplify();
		final IExpressionList after = simplifyAddends();

		return finishSimplification(after);
	}

	public final Expression innerStepSimplify() {
		if (addends.isSingle())
			return addends.getFirst().innerStepSimplify();
		final IExpressionList after = stepSimplifyAddends();

		if (!after.equals(addends))
			return new Sum(after);

		return finishSimplification(after);
	}

	private static Expression orderOrDefault(final Expression after) {
		final Expression toret = order(after);
		if (toret != null)
			return toret;
		return after;
	}

	private static Expression orderOrDefault(final IExpressionList after) {
		final Expression toret = order(after);
		if (toret != null)
			return toret;
		return new Sum(after);
	}

	private Expression finishSimplification(final IExpressionList after) {
		final IIterator<Expression> iterator = after.getIterator();
		final Expression inner = innerSum(after, iterator);

		if (inner != null)
			return orderOrDefault(inner);
		if (IIterable.isEmpty(after))
			return Int.ZERO;

		return orderOrDefault(after);
	}

	private Expression innerSum(final IExpressionList after, final IIterator<Expression> iterator) {
		final Expression binomialSquare = tryBinomialSquare();
		if (binomialSquare != null)
			return binomialSquare;
		while (iterator.hasNext()) {
			final Expression first = iterator.next();
			for (Expression second : iterator) {
				if (first.isZero()) {
					return finishInner(after, first, second, second);
				}

				if (second.isZero()) {
					return finishInner(after, first, second, first);
				}

				if (first.equals(second)) {
					final Expression toret = Int.TWO.multiply(first);
					return finishInner(after, first, second, toret);
				}

				if (first.isOpposite(second)) {
					final Expression toret = Int.ZERO;
					return finishInner(after, first, second, toret);
				}

				if (first.compatible(second, Operation.ADDITION)) {
					final Expression toret = first.add(second);
					return finishInner(after, first, second, toret);
				}

				if (second.compatible(first, Operation.ADDITION)) {
					final Expression toret = second.add(first);
					return finishInner(after, first, second, toret);
				}
			}
		}
		return null;
	}

	private boolean isDivisible(final Expression elementAt) {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ICalculable curr = iterator.next();
			if (!curr.compatible(elementAt, Operation.DIVISION))
				return false;
		}
		return true;
	}

	public final boolean isFullDouble() {
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			if (!iterator.next().isFullDouble())
				return false;
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	public final boolean isOne() {
		return addends.isSingle() && addends.getFirst().isOne();
	}

	/**
	 * {@inheritDoc}
	 */
	public final boolean isZero() {
		return IIterable.isEmpty(addends) || addends.isSingle() && addends.getFirst().isZero();
	}

	public final ITensor matrixDvalue(final char letter, final double value) {
		final IIterator<Expression> iterator = getIterator();
		ITensor toret = Int.ZERO;
		while (iterator.hasNext()) {
			final ITensor dVal = iterator.next().matrixDvalue(letter, value);
			toret = toret.add(dVal);
		}
		return toret;
	}

	public final ITensor matrixDvalue(final IValueList valueList) {
		final IIterator<Expression> iterator = getIterator();
		ITensor toret = Int.ZERO;
		while (iterator.hasNext()) {
			final ITensor dVal = iterator.next().matrixDvalue(valueList);
			toret = toret.add(dVal);
		}
		return toret;
	}

	/**
	 * {@inheritDoc}
	 */
	public final Expression multiply(final Expression arg) {
		if (equals(arg))
			return super.multiply(arg);
		if (arg instanceof Sum) {
			final IExpressionList toret = new ExpressionList();
			final IIterator<Expression> iterator = getIterator();
			while (iterator.hasNext()) {
				final ICalculable current = iterator.next();
				final IIterator<Expression> innerIterator = ((Sum) arg).getIterator();
				while (innerIterator.hasNext())
					toret.add(current.multiply(innerIterator.next()));
			}
			return new Sum(toret);
		}
		if (arg instanceof INumber) {
			final IExpressionList toret = new ExpressionList();
			final IIterator<Expression> iterator = getIterator();
			while (iterator.hasNext())
				toret.add(iterator.next().multiply(arg));
			return new Sum(toret);
		}
		return super.multiply(arg);
	}

	public final boolean needParenthesis() {
		return !addends.isSingle();
	}

	/**
	 * {@inheritDoc}
	 */
	public final Expression opposite() {
		final IExpressionList after = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ICalculable curr = iterator.next();
			final Expression currs = curr.opposite();
			if (currs.isZero())
				continue;
			if (currs instanceof Sum)
				after.addRange(((Sum) currs).addends);
			else
				after.add(currs);
		}
		return new Sum(after);
	}

	private Expression order() {
		final Expression toret = order((IIterable<Expression>) this);
		if (toret == null)
			return this;
		return toret;
	}

	public final Expression partialSubstitute(final char letter, final double value) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret.add(iterator.next().partialSubstitute(letter, value));
		return new Sum(toret);
	}

	public final Expression partialSubstitute(final char letter, final Expression value) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret.add(iterator.next().partialSubstitute(letter, value));
		return new Sum(toret);
	}

	public final Expression partialSubstitute(final IValueList list) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret.add(iterator.next().partialSubstitute(list));
		return new Sum(toret);
	}

	private IExpressionList simplifyAddends() {
		final IExpressionList after = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ISimplifiable curr = iterator.next();
			final Expression currs = curr.partialSimplify();
			if (currs.isZero())
				continue;
			if (currs instanceof Sum)
				after.addRange(((Sum) currs).addends);
			else if (currs instanceof Multiplication)
				after.add(currs.expand());
			else
				after.add(currs);
		}
		return after;
	}

	private IExpressionList stepSimplifyAddends() {
		final IExpressionList after = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			final ISimplifiable curr = iterator.next();
			final Expression currs = curr.innerStepSimplify();
			if (currs.isZero())
				continue;
			if (currs instanceof Sum)
				after.addRange(((Sum) currs).addends);
			else if (currs instanceof Multiplication)
				after.add(currs.expand());
			else
				after.add(currs);
		}
		return after;
	}

	public final void toFullString(final StringBuffer buffer) {
		buffer.append("+(");
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext()) {
			iterator.next().toFullString(buffer);
			if (iterator.hasNext())
				buffer.append(',');
		}
		buffer.append(')');
	}

	public final void toString(final StringBuffer buffer) {
		boolean first = true;
		for (Expression curr : this) {
			if (!first && !curr.toStringStartsWith('-'))
				buffer.append('+');
			curr.toString(buffer);
			first = false;
		}
	}

	public boolean toStringStartsWith(char prefix) {
		final IIterator<Expression> iterator = getIterator();
		return iterator.hasNext() && iterator.next().toStringStartsWith('-');
	}

	private Expression tryBinomialSquare() {
		if (IIterable.length(addends) != 3)
			return null;

		final IIterator<Expression> iterator = getIterator();
		ICalculable remainder = null;
		ICalculable left = null;
		Expression right = null;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			final Expression root = trySquare(curr);
			if (root == null) {
				if (remainder == null) {
					remainder = curr;
					continue;
				}
				return null;
			}
			if (left == null) {
				left = root;
				continue;
			}
			if (right != null)
				return null;
			right = root;
			if (remainder == null)
				remainder = iterator.next();
		}

		if (left == null || right == null || remainder == null)
			return null;

		final Expression leftXright = SimplificationHelper.simplify(left.multiply(right).multiply(Int.TWO));
		if (leftXright.equals(remainder))
			return new Power(left.add(right), Int.TWO);
		if (leftXright.equals(SimplificationHelper.simplify(remainder.opposite())))
			return new Power(left.add(right.opposite()), Int.TWO);

		return null;
	}

	public final INumber value(final IValueList list) {
		final IIterator<Expression> iterator = getIterator();
		INumber toret = Int.ZERO;
		while (iterator.hasNext())
			toret = toret.add(iterator.next().value(list));
		return toret;
	}

	public boolean isIdentical(final Expression other) {
		if (!(other instanceof Sum))
			return false;
		final Sum oth = (Sum) other;
		return areIdentical(getIterator(), oth.getIterator());
	}

	public Expression accept(final IExpressionVisitor visitor) {
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer) {
		boolean first = true;
		for (Expression curr : this) {
			if (!first && !curr.toStringStartsWith('-'))
				buffer.append('+');
			curr.toWrappedHtml(buffer);
			first = false;
		}
	}
}
