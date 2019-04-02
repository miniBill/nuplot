package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.list.ExpressionList;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.other.Poly;
import meplot.expressions.visitors.IExpressionVisitor;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.persistence.Settings;
import platform.log.Log;

public final class Division extends AbstractExpression implements IDivision {
	private static class FakeMultiplication implements IMultiplication {
		private final Expression arg;

		FakeMultiplication(final Expression arg) {
			this.arg = arg;
		}

		public IIterator<Expression> getIterator() {
			return new ExpressionList(arg).getIterator();
		}

		public INumber coefficent() {
			if (arg instanceof INumber)
				return (INumber) arg;
			return Int.ONE;
		}

		@Override
		public boolean contains(Expression arg, int start) {
			return start == 0 && this.arg.equals(arg);
		}
	}

	public static final Division EMPTY = new Division(Int.ZERO, Int.ZERO);

	private final Expression numerator;
	private final Expression denominator;

	public Division(final Expression num, final Expression den) {
		numerator = num;
		denominator = den;
	}

	public Expression innerSimplify() {
		final Expression numeratorS = numerator.partialSimplify();
		final Expression denominatorS = denominator.partialSimplify();
		return finishSimplification(numeratorS, denominatorS);
	}

	private static Expression finishSimplification(final Expression numeratorS, final Expression denominatorS) {
		if (numeratorS instanceof IReal && denominatorS instanceof IReal) {
			IReal rnumerator = (IReal) numeratorS;
			IReal rdenominator = (IReal) denominatorS;
			return rnumerator.divide(rdenominator);
		}
		if (numeratorS.compatible(denominatorS, Operation.DIVISION))
			return numeratorS.divide(denominatorS);
		if (denominatorS.isOne())
			return numeratorS;
		if (numeratorS.isZero())
			return Int.ZERO;
		if (numeratorS instanceof IReal && ((IReal) numeratorS).isNegative() && denominatorS instanceof Sum)
			return new Division(((IReal) numeratorS).iropposite(), denominatorS.opposite());
		if (numeratorS.equals(SimplificationHelper.simplify(denominatorS.opposite()))
				|| SimplificationHelper.simplify(numeratorS.opposite()).equals(denominatorS))
			return Int.MINUSONE;
		if (denominatorS instanceof IReal && ((IReal) denominatorS).isNegative())
			return new Division(numeratorS.opposite(), ((IReal) denominatorS).iropposite());
		if (denominatorS instanceof IMultiplication
				&& ((IMultiplication) denominatorS).coefficent().real().isNegative())
			return new Division(numeratorS.opposite(), denominatorS.opposite());
		if (denominatorS instanceof IComplex)
			return ((IComplex) denominatorS).invdivide(numeratorS);
		if (numeratorS instanceof IDivision)
			return finishSimplificationNumDivision((IDivision) numeratorS, denominatorS);
		if (denominatorS instanceof IDivision)
			return finishSimplificationDenDivision(numeratorS, (IDivision) denominatorS);
		if (numeratorS instanceof Sum && denominatorS instanceof Sum)
			return simplifyRoots((Sum) numeratorS, (Sum) denominatorS);
		if (numeratorS instanceof IMultiplication)
			return finishSimplificationNumIMultiplication((IMultiplication) numeratorS, denominatorS);
		if (denominatorS instanceof Power)
			return finishSimplificationDenPower(numeratorS, (Power) denominatorS);
		if (denominatorS instanceof IMultiplication)
			return finishSimplificationDenIMultiplication(numeratorS, (IMultiplication) denominatorS);
		if (denominatorS instanceof Sqrt)
			return new Division(numeratorS.multiply(denominatorS), denominatorS.square());
		return new Division(numeratorS, denominatorS);
	}

	private static Expression finishSimplificationDenPower(final Expression numeratorS, final Power pds) {
		if (pds.getBase().equals(numeratorS))
			return new Power(numeratorS, pds.getExponent().add(Int.MINUSONE));
		return new Division(numeratorS, pds);
	}

	private static Expression finishSimplificationNumDivision(final IDivision divNumerator,
			final Expression denominatorS) {
		return new Division(divNumerator.getNumerator(), divNumerator.getDenominator().multiply(denominatorS));
	}

	private static Expression finishSimplificationDenDivision(final ICalculable numeratorS,
			final IDivision divDenominator) {
		return new Division(numeratorS.multiply(divDenominator.getDenominator()), divDenominator.getNumerator());
	}

	public Expression innerStepSimplify() {
		final Expression numeratorS = numerator.innerStepSimplify();
		final Expression denominatorS = denominator.innerStepSimplify();
		if (!numeratorS.equals(numerator) || !denominatorS.equals(denominator))
			return new Division(numeratorS, denominatorS);
		return finishSimplification(numeratorS, denominatorS);
	}

	private static Expression finishSimplificationNumIMultiplication(final IMultiplication numeratorS,
			final Expression denominatorS) {
		if (denominatorS instanceof IMultiplication)
			return crossSimplify(numeratorS, (IMultiplication) denominatorS);
		return crossSimplify(numeratorS, new FakeMultiplication(denominatorS));
	}

	private static boolean tryCross;

	private static Expression simplifyRoots(final Sum nums, final Sum dens) {
		if (!tryCross)
			return new Division(nums, dens);
		for (char variable = 'a'; variable <= 'z'; variable++)
			if (nums.hasLetter(variable) && dens.hasLetter(variable) && Poly.isPoly(nums, variable)
					&& Poly.isPoly(dens, variable)) {
				final Poly pnum = new Poly(nums, variable);
				final Poly pden = new Poly(dens, variable);
				final Poly gcd = FunctionsMath.gcd(pnum, pden);
				if (!gcd.isOne()) {
					Expression ndiv = nums;
					Expression ddiv = dens;
					try {
						ndiv = pnum.pdivide(gcd);
						ndiv = SimplificationHelper.simplify(ndiv);
						ddiv = pden.pdivide(gcd);
						ddiv = SimplificationHelper.simplify(ddiv);
						return new Division(ndiv, ddiv);
					} catch (final CalcException e) {
						Log.log(e);
					}
					if (ndiv instanceof Division || ddiv instanceof Division)
						throw new CalcException("Something went wrong in simplifyRoots");
					return new Division(nums, dens);
				}
			}
		return new Division(nums, dens);
	}

	private static Expression finishSimplificationDenIMultiplication(final Expression numerator,
			final IMultiplication denominator) {
		return crossSimplify(new Multiplication(numerator), denominator);
	}

	private static Expression crossSimplify(final IMultiplication numerator, final IMultiplication denominator) {
		final IExpressionList newNumList = new ExpressionList();
		final IIterator<Expression> nFactorsIt = numerator.getIterator();
		while (nFactorsIt.hasNext()) {
			final Expression currentN = nFactorsIt.next();
			final IExpressionList newDenList = new ExpressionList();
			final IIterator<Expression> dFactorsIt = denominator.getIterator();
			while (dFactorsIt.hasNext()) {
				final Expression currentD = dFactorsIt.next();
				if (currentN.compatible(currentD, Operation.DIVISION)) {
					final Expression test = currentN.divide(currentD);
					if (!(test instanceof Fraction)) {
						newNumList.add(test);
						newNumList.addRange(nFactorsIt);
						newDenList.addRange(dFactorsIt);
						return new Division(new Multiplication(newNumList), new Multiplication(newDenList));
					}
				}
				newDenList.add(currentD);
			}
			newNumList.add(currentN);
		}

		final Expression nnum;
		if (numerator.length() == 1)
			nnum = numerator.getFirst();
		else if (numerator instanceof Expression)
			nnum = (Expression) numerator;
		else
			throw new CalcException("Awfully wrong numerator in Division#crossSimplify");

		final Expression nden;
		if (denominator.length() == 1)
			nden = denominator.getFirst();
		else if (denominator instanceof Expression)
			nden = (Expression) denominator;
		else
			throw new CalcException("Awfully wrong numerator in Division#crossSimplify");
		return new Division(nnum, nden);
	}

	public boolean compatible(final Expression elementAt, final char operation) {
		return true;
	}

	public Expression add(final Expression other) {
		if (other instanceof IDivision) {
			final IDivision otherd = (IDivision) other;
			if (otherd.getDenominator().equals(denominator))
				return new Division(numerator.add(otherd.getNumerator()), denominator);
			final Expression lcm = FunctionsMath.lcm(denominator, otherd.getDenominator());
			final Expression thisMul = SimplificationHelper.simplify(lcm.divide(denominator));
			final Expression othMul = SimplificationHelper.simplify(lcm.divide(otherd.getDenominator()));
			return new Division(numerator.multiply(thisMul).add(otherd.getNumerator().multiply(othMul)), lcm);
		}
		return new Division(numerator.add(denominator.multiply(other)), denominator);
	}

	public Expression multiply(final Expression other) {
		return new Division(numerator.multiply(other), denominator);
	}

	public Expression divide(final Expression other) {
		return new Division(numerator, denominator.multiply(other));
	}

	public void toString(final StringBuffer buffer) {
		numerator.toPString(buffer);
		buffer.append(Operation.DIVISION);
		if (denominator instanceof Division)
			buffer.append('(');
		denominator.toPString(buffer);
		if (denominator instanceof Division)
			buffer.append(')');
	}

	public INumber value(final IValueList arg) {
		final INumber nval = numerator.value(arg);
		final INumber dval = denominator.value(arg);
		return nval.divide(dval);
	}

	public Expression partialSubstitute(final IValueList valueList) {
		return new Division(numerator.partialSubstitute(valueList), denominator.partialSubstitute(valueList));
	}

	public Expression partialSubstitute(final char letter, final double value) {
		return new Division(numerator.partialSubstitute(letter, value), denominator.partialSubstitute(letter, value));
	}

	public Expression partialSubstitute(final char letter, final Expression value) {
		return new Division(numerator.partialSubstitute(letter, value), denominator.partialSubstitute(letter, value));
	}

	public boolean hasLetter(final char letter) {
		return numerator.hasLetter(letter) || denominator.hasLetter(letter);
	}

	public boolean containsMatrix() {
		return numerator.containsMatrix() || denominator.containsMatrix();
	}

	public void toFullString(final StringBuffer buffer) {
		buffer.append("/(");
		numerator.toFullString(buffer);
		buffer.append(',');
		denominator.toFullString(buffer);
		buffer.append(')');
	}

	public Expression getDenominator() {
		return denominator;
	}

	public Expression getNumerator() {
		return numerator;
	}

	public void changedSetting(final String name, final int arg) {
		if (name.equals(Settings.TRYCROSS))
			tryCross = arg == 1;
	}

	public boolean equals(final Object obj) {
		if (obj instanceof Division) {
			final Division other = (Division) obj;
			final Expression nod = numerator.multiply(other.getDenominator());
			final Expression don = other.numerator.multiply(denominator);
			return nod.equals(don);
		}
		return super.equals(obj);
	}

	public Expression expand() {
		return new Division(numerator.expand(), denominator.expand());
	}

	public double fdvalue(final char letter, final double value) {
		return numerator.fdvalue(letter, value) / denominator.fdvalue(letter, value);
	}

	public boolean isFullDouble() {
		return numerator.isFullDouble() && denominator.isFullDouble();
	}

	public boolean isIdentical(final Expression other) {
		if (!(other instanceof Division))
			return false;
		final Division oth = (Division) other;
		return numerator.isIdentical(oth.numerator) && denominator.isIdentical(oth.denominator);
	}

	public Expression accept(final IExpressionVisitor visitor) {
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer) {
		numerator.toWrappedHtml(buffer);
		buffer.append('/');
		denominator.toWrappedHtml(buffer);
	}
}
