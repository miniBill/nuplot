package meplot.expressions.other;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.exceptions.SimplificationException;
import meplot.expressions.list.ExpressionList;
import platform.lists.IIterable;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.operations.Power;
import meplot.expressions.operations.Sum;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import platform.log.Log;
import platform.log.LogLevel;

public final class Poly extends Sum {
	private final char var;
	private final Letter varLetter;

	public Poly(final Sum original, final char var) {
		super(original.getIterator());
		this.var = var;
		varLetter = new Letter(var);
	}

	public Poly(final Expression expr, final char var) {
		super(expr);
		this.var = var;
		varLetter = new Letter(var);
	}

	private Poly(final IExpressionList addends, final char var) {
		super(addends);
		this.var = var;
		varLetter = new Letter(var);
	}

	public static boolean isPoly(final Sum poly, final char var) {
		final IIterator<Expression> iterator = poly.getIterator();
		while (iterator.hasNext())
			if (!isMonomial(iterator.next(), var))
				return false;
		return true;
	}

	private static boolean isMonomial(final Expression next, final char var) {
		if (isSimple(next))
			return true;
		if (!next.hasLetter(var))
			return true;
		if (next instanceof Monomial)
			return true;
		if (next instanceof Multiplication) {
			final IIterator<Expression> iterator = ((Multiplication) next).getIterator();
			while (iterator.hasNext())
				if (!isMonomial(iterator.next(), var))
					return false;
			return true;
		}
		if (next instanceof Power) {
			final Power power = (Power) next;
			return power.getBase() instanceof Letter && power.getExponent() instanceof Int;
		}
		if (next instanceof Sum) {
			final IIterator<Expression> iterator = ((Sum) next).getIterator();
			while (iterator.hasNext())
				if (!isMonomial(iterator.next(), var))
					return false;
			return true;
		}
		return false;
	}

	private static boolean isSimple(final Expression next) {
		return next instanceof Int || next instanceof Fraction || next instanceof Letter || isPower(next);
	}

	private static boolean isPower(final Expression next) {
		if (!(next instanceof Power)) {
			return false;
		}
		final Power power = (Power) next;
		return power.getExponent() instanceof Int && power.getBase() instanceof Letter;
	}

	public Expression getLeadingCoeff() {
		return getCoefficent(getDegree());
	}

	private int degree = -1;

	public int getDegree() {
		if (degree < 0) {
			degree = 0;
			final IIterator<Expression> iterator = getIterator();
			while (iterator.hasNext()) {
				final int exp = getExponent(iterator.next());
				if (exp > degree)
					degree = exp;
			}
		}
		return degree;
	}

	public static Expression getCoefficent(final Expression expr, final char var) {
		if (expr instanceof INumber)
			return expr;
		if (expr instanceof Multiplication) {
			final IIterator<Expression> iterator = ((Multiplication) expr).getIterator();
			Expression toret = Int.ONE;
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				toret = toret.multiply(getCoefficent(curr, var));
			}
			return toret;
		}
		if (expr instanceof Power) {
			final Power power = (Power) expr;
			if (power.getBase() instanceof Letter) {
				if (((Letter) power.getBase()).getLetter() == var)
					return Int.ONE;
				return expr;
			}
		}
		if (expr instanceof Letter) {
			if (((Letter) expr).getLetter() == var)
				return Int.ONE;
			return expr;
		}

		if (expr instanceof Division) {
			final Division div = (Division) expr;
			final Expression num = div.getNumerator();
			final Expression den = div.getDenominator();
			return getCoefficent(num, var).divide(getCoefficent(den, var));
		}

		if (expr instanceof Monomial)
			return ((Monomial) expr).getCoefficent();

		if (expr instanceof Sum) {
			final Sum sexpr = (Sum) expr;
			final IIterator<Expression> iterator = sexpr.getIterator();
			Expression toret = Int.ZERO;
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				if (curr.hasLetter(var))
					toret = toret.add(getCoefficent(curr, var));
			}
			return toret;
		}

		if (expr.hasLetter(var))
			Log.log(LogLevel.WARNING, "Returning expr from getCoefficent of " + expr + " var was " + var);

		return expr;
	}

	private int getExponent(final Expression expr) {
		return getExponent(expr, var);
	}

	private static int extractExponent(final Power expr, final char var) {
		if (expr.getBase() instanceof Letter) {
			if (((Letter) expr.getBase()).getLetter() != var)
				return 0;
			if (expr.getExponent() instanceof Int)
				return ((Int) expr.getExponent()).getValue();
		}
		return -1;
	}

	public Expression getTrailingCoeff() {
		return getCoefficent(0);
	}

	public Expression getCoefficent(final int deg) {
		final IIterator<Expression> iterator = getIterator();
		Expression toret = Int.ZERO;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			final int exp = getExponent(curr);
			if (exp == deg)
				toret = toret.add(getCoefficent(curr, var));
		}
		return toret;
	}

	public static boolean isPoly(final Expression arg, final char var) {
		if (arg instanceof Sum)
			return isPoly((Sum) arg, var);
		return isPoly(new Sum(arg), var);
	}

	public Letter getLetter() {
		return varLetter;
	}

	public Expression pdivide(final Poly arg) {
		if (equals(arg))
			return Int.ONE;
		Poly temp = this;
		Poly toret = new Poly(Int.ZERO, var);
		final int divdeg = arg.getDegree();
		if (divdeg == 0 && IIterable.length(arg) == 1) {
			final Expression single = arg.getFirst();
			if (single instanceof INumber) {
				final IExpressionList results = new ExpressionList();
				final IIterator<Expression> iterator = getIterator();
				while (iterator.hasNext())
					results.add(iterator.next().divide(single));
				return new Sum(results);
			}
		}
		final Monomial olead = arg.getLeadingTerm();
		temp = temp.psimplify();
		while (!temp.isZero() && temp.getDegree() >= divdeg) {
			final Monomial lead = temp.getLeadingTerm();
			final Monomial div = lead.divide(olead).msimplify();
			toret = toret.add(div);
			if (toret == null)
				throw new CalcException("Something went wrong in pdivide, toret");
			final Poly tosub = arg.multiply(div).ppartialSimplify();
			if (tosub == null)
				throw new CalcException("Something went wrong in pdivide, tosub");
			final Poly subbed = temp.add(tosub.popposite()).ppartialSimplify();
			if (subbed == null)
				throw new CalcException("Something went wrong in pdivide, sub");
			final Poly sim = subbed.psimplify();
			if (sim.getDegree() >= temp.getDegree() && sim.getDegree() > 0) {
				subbed.psimplify();
				throw new CalcException("Something went wrong in pdivide");
			}
			temp = sim.psimplify();
		}
		return toret;
	}

	private Poly ppartialSimplify() {
		final Expression sim = partialSimplify();
		if (!isPoly(sim, var))
			throw new SimplificationException("partialSimplify went wrong in Poly");
		if (sim instanceof Poly)
			return (Poly) sim;
		return new Poly(sim, var);
	}

	public Poly psimplify() {
		final Expression sim = SimplificationHelper.simplify(this);
		if (sim instanceof Poly)
			return (Poly) sim;
		if (!isPoly(sim, var)) {
			SimplificationHelper.simplify(sim);
			throw new CalcException("Simplified poly isn't poly");
		}
		return new Poly(sim, var);
	}

	private Poly popposite() {
		final IExpressionList after = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();

		while (iterator.hasNext()) {
			final ICalculable curr = iterator.next();
			final Expression currs = curr.opposite();
			if (currs.isZero())
				continue;
			if (currs instanceof Sum) {
				final Sum scls = (Sum) currs;
				after.addRange(scls.getIterator());
			} else
				after.add(currs);
		}
		return new Poly(after, var);
	}

	private Poly add(final Poly arg) {
		return new Poly(new ExpressionList(getIterator(), arg.getIterator()), var);
	}

	private Poly multiply(final Monomial arg) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = getIterator();
		while (iterator.hasNext())
			toret.add(iterator.next().multiply(arg));
		return new Poly(toret, var);
	}

	public Poly mod(final Poly arg) {
		Poly temp = this;
		final int divdeg = arg.getDegree();
		final Monomial olead = arg.getLeadingTerm();
		while (temp.getDegree() >= divdeg) {
			final Monomial lead = temp.getLeadingTerm();
			final Monomial div = lead.divide(olead).mopposite();
			final Poly tosub = arg.multiply(div);
			final Poly subbed = temp.add(tosub);
			final Poly sim = subbed.psimplify();
			if (sim.getDegree() == 0)
				return new Poly(sim, temp.var);
			if (sim.getDegree() == temp.getDegree()) {
				final Expression stupid = sim.psimplify();
				throw new CalcException("Mod didn't reduce degree of poly, stupid:" + stupid);
			}
			temp = sim;
		}
		return temp;
	}

	public Monomial getLeadingTerm() {
		return getTerm(getDegree());
	}

	private Monomial getTerm(final int deg) {
		final IIterator<Expression> iterator = getIterator();
		Monomial toret = new Monomial(Int.ZERO, var);
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			final int exp = getExponent(curr);
			if (exp == deg)
				toret = toret.add(new Monomial(curr, var));
		}
		return toret;
	}

	private Poly add(final Monomial arg) {
		final Expression result = super.add(arg);
		if (result instanceof Poly)
			return (Poly) result;
		if (isPoly(result, var))
			return new Poly(result, var);
		throw new CalcException("Trying to add poly with nonpoly");
	}

	public static int getExponent(final Expression expr, final char var) {
		if (expr instanceof INumber)
			return 0;
		if (expr instanceof Multiplication) {
			final IIterator<Expression> iterator = ((Multiplication) expr).getIterator();
			int toret = 0;
			while (iterator.hasNext()) {
				final Expression curr = iterator.next();
				toret += getExponent(curr, var);
			}
			return toret;
		}
		if (expr instanceof Monomial)
			return ((Monomial) expr).getExponent();
		if (expr instanceof Power)
			return extractExponent((Power) expr, var);
		if (expr instanceof Letter && ((Letter) expr).getLetter() == var)
			return 1;
		return 0;
	}

	public char getVar() {
		return var;
	}

	public Poly sumExpand() {
		final IIterator<Expression> iterator = getIterator();
		final ExpressionList toret = new ExpressionList();
		boolean same = true;
		while (iterator.hasNext()) {
			final Expression curr = iterator.next();
			if (curr instanceof Sum) {
				same = false;
				toret.addRange(((Sum) curr).getIterator());
			} else
				toret.add(curr);
		}
		if (same)
			return this;
		return new Poly(toret, var);
	}
}
