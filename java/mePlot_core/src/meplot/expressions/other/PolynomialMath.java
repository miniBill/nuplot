package meplot.expressions.other;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.DivisorException;
import meplot.expressions.list.ExpressionList;
import platform.lists.IIterable;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.operations.Power;
import meplot.expressions.operations.Sum;
import meplot.expressions.visitors.simplification.SimplificationHelper;

import java.util.Iterator;

public final class PolynomialMath {
	private PolynomialMath() {
	}

	public static IExpressionList getRoots(final Expression poly, final char var) {
		if (poly instanceof Poly)
			return getRoots((Poly) poly, var);
		if (poly instanceof Sum && Poly.isPoly(poly, var))
			return getRoots(new Poly((Sum) poly, var), var);
		if (poly instanceof Letter) {
			if (((Letter) poly).getLetter() == var)
				return new ExpressionList(Int.ZERO);
			return ExpressionList.getEmpty();
		}
		if (poly instanceof INumber)
			return ExpressionList.getEmpty();
		if (poly instanceof Power) {
			final Power ppoly = (Power) poly;
			final Expression exp = ppoly.getExponent();
			if (exp instanceof IInt) {
				final IInt iexp = (IInt) exp;
				final int value = iexp.getValue();
				if (value <= 0)
					return ExpressionList.getEmpty();
				return getRoots(ppoly.getBase(), var);
			}
		}
		return ExpressionList.getEmpty();
	}

	private static IExpressionList getRoots(final Poly poly, final char var) {
		if (!Poly.isPoly(poly, var))
			return ExpressionList.getEmpty();
		final Expression lead = poly.getLeadingCoeff();
		final IExpressionList leadDiv = getDivisors(lead);
		if (leadDiv == null)
			return ExpressionList.getEmpty();
		final Expression trail = poly.getTrailingCoeff();
		final IExpressionList trailDiv = getDivisors(trail);
		final ExpressionList toret = new ExpressionList();
		for (Expression leadCurr : leadDiv) {
			for (Expression trailCurr : trailDiv) {
				final Expression possibleRoot = SimplificationHelper.simplify(trailCurr.divide(leadCurr));
				if (isRoot(possibleRoot, poly, var))
					toret.add(possibleRoot);
				final Expression possibleRootOpp = possibleRoot.opposite();
				if (isRoot(possibleRootOpp, poly, var))
					toret.add(possibleRootOpp);
			}
		}
		return toret;
	}

	private static boolean isRoot(final Expression test, final Poly poly, final char var) {
		final Expression sub = poly.partialSubstitute(new ValueList(var, test));
		return SimplificationHelper.simplify(sub).isZero();
	}

	private static IExpressionList getDivisors(final Expression expr) {
		if (expr instanceof Letter)
			return new ExpressionList(expr);
		if (expr instanceof IInt)
			return divisors((IInt) expr);
		if (expr instanceof Fraction)
			return divisors((Fraction) expr);
		if (expr instanceof Multiplication)
			return divisors((Multiplication) expr);
		if (expr instanceof Power)
			return divisors((Power) expr);
		throw new DivisorException("Asked for divisors of " + expr.toFullString() + " what should I do?");
	}

	private static IExpressionList divisors(final Multiplication expr) {
		final IExpressionList[] gens = new IExpressionList[IIterable.length(expr)];
		final Iterator<Expression> factors = expr.iterator();
		gen(factors, gens);
		final int[] indexes = new int[gens.length];
		final IExpressionList toret = new ExpressionList();
		while (true) {
			Expression toadd = Int.ONE;
			for (int i = 0; i < indexes.length; i++)
				toadd = toadd.multiply(gens[i].elementAt(indexes[i]));
			toret.add(toadd);

			for (int j = 0; j < indexes.length; j++) {
				indexes[j]++;
				if (indexes[j] == IIterable.length(gens[j])) {
					if (j == indexes.length - 1)
						return toret.fold();
					indexes[j] = 0;
				}
			}
		}
	}

	private static void gen(final Iterator<Expression> iterator, final IExpressionList[] gens) {
		for (int i = 0; i < gens.length; i++)
			gens[i] = new ExpressionList(Int.ONE, getDivisors(iterator.next()).iterator()).fold();
	}

	private static/* @Nullable */IExpressionList divisors(final Power power) {
		if (power.getExponent() instanceof Int) {
			final Expression base = power.getBase();
			Expression current = base;
			final IExpressionList toret = new ExpressionList(Int.ONE);
			final int max = ((Int) power.getExponent()).getValue();
			for (int i = 1; i <= max; i++) {
				toret.add(current);
				if (i < max)
					current = current.multiply(base);
			}
			return toret;
		}
		throw new DivisorException("Asked for divisors of power with nonint exp");
	}

	public static IExpressionList divisors(final Fraction frac) {
		final IInt num = frac.fgetNumerator();
		final IInt den = frac.fgetDenominator();
		final IExpressionList numDiv = divisors(num);
		final IExpressionList denDiv = divisors(den);
		final IExpressionList toret = new ExpressionList();
		for (Expression denCurr : denDiv)
			for (Expression numCurr : numDiv)
				toret.add(SimplificationHelper.simplify(numCurr.divide(denCurr)));
		return toret.fold();
	}

	public static IExpressionList divisors(final IInt dint) {
		final IExpressionList toret = new ExpressionList();
		toret.add(Int.ONE);
		final int val = Math.abs(dint.getValue());
		if ((val & 1) == 0)
			toret.add(Int.TWO);
		for (int i = 3; i <= val; i++)
			if (val % i == 0)
				toret.add(new Int(i));
		return toret;
	}

	/*
	 * public static Expression divide(final Poly num, final Poly den){ Expression
	 * toret = Int.ZERO; final ExpressionIterator ncoeff = num.getCoefficents();
	 * final ExpressionIterator dcoeff = den.getCoefficents(); if(dcoeff.length() ==
	 * 2){ final Expression first = dcoeff.next(); final Expression second =
	 * dcoeff.next(); final Expression root =
	 * first.divide(second).opposite().simplify(); final Expression[] narr =
	 * ncoeff.toArray(); Expression last = narr[narr.length - 1]; toret =
	 * narr[narr.length - 1].multiply(new Power(num.getLetter(), narr.length - 2));
	 * for(int i = 1; i < narr.length - 1; i++){ final Expression expr = new
	 * Power(num.getLetter(), narr.length - 2 - i); final Expression coeff =
	 * narr[narr.length - i - 1].add(last); toret = toret.add(coeff.multiply(expr));
	 * last = coeff.multiply(root); } if(!last.add(narr[0]).simplify().isZero()){
	 * throw new CalcException("Division unsuccessfull. Remainder nonzero"); }
	 * return toret.multiply(second); } return toret; }
	 */
}
