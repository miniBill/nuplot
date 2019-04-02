package meplot.solver;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.numbers.Real;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.Power;
import meplot.expressions.other.Poly;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public final class PaidSolver extends AbstractSolver {
	private static char getNewKind(final char kind, final Expression coeff) {
		if (kind == EQUALS)
			return EQUALS;
		if (coeff instanceof Real) {
			final Real rca = (Real) coeff;
			if (rca.isNegative())
				return invert(kind);
			return kind;
		}
		return Operation.UNKNOWN;
	}

	private static char invert(final char nkind) {
		if (nkind == Operation.LESS)
			return Operation.GREATER;
		if (nkind == Operation.GREATER)
			return Operation.LESS;
		if (nkind == Operation.GEQ)
			return Operation.LEQ;
		if (nkind == Operation.LEQ)
			return Operation.GEQ;
		return '?';
	}

	protected boolean canSolve(final char kind) {
		switch (kind) {
		case EQUALS:
		case Operation.LESS:
		case Operation.LEQ:
		case Operation.GEQ:
		case Operation.GREATER:
			return true;
		default:
			return false;
		}
	}

	protected void poly1(final ExpressionTree sim, final Poly poly, final char kind) {
		final Expression coeffA = poly.getCoefficent(1);
		final Expression coeffB = poly.getCoefficent(0);
		final Expression coeffAX = SimplificationHelper.simplify(coeffA.multiply(poly.getLetter()));
		final Expression firstep = new BooleanOp(coeffAX.add(coeffB), kind, Int.ZERO);
		final Expression minusb = SimplificationHelper.simplify(coeffB.opposite());
		final Expression secstep = new BooleanOp(coeffAX, kind, minusb);
		final ExpressionTree firstleaf = sim.addChild(firstep);
		final ExpressionTree secleaf = firstleaf.addChild(secstep);
		if (coeffA.isZero()) {
			if (coeffB.isZero())
				secleaf.addChild(Letter.FORALL);
			else
				secleaf.addChild(Letter.NOTEXISTS);
			return;
		}
		final Expression res1 = SimplificationHelper.simplify(minusb.divide(coeffA));
		final char nkind = getNewKind(kind, coeffA);
		secleaf.addChild(new BooleanOp(poly.getLetter(), nkind, res1));
	}

	protected void poly2(final ExpressionTree sim, final Poly poly, final char kind) {
		final Expression coeffA = poly.getCoefficent(2);
		final Expression coeffB = poly.getCoefficent(1);
		final Expression coeffC = poly.getCoefficent(0);
		if (coeffB.isZero()) {
			poly2zerob(sim, poly, kind, coeffA, coeffC);
			return;
		}

		// (k) can be <, >, <=, >=, =
		// ax^2+bx+c (k) 0
		final ExpressionTree leaf = poly2addfirstep(sim, poly, coeffA, coeffB, coeffC, kind);

		// mfac = -4ac
		final Expression mfac = Int.FOUR.multiply(coeffA).multiply(coeffC).opposite();
		// d = b^2-4ac
		final Expression coeffD = new Power(coeffB, Int.TWO).add(mfac);
		// e = sqrt(b^2-4ac)
		final Expression coeffE = new Sqrt(coeffD);
		// mb = -b
		final ICalculable coeffMinusB = coeffB.opposite();
		// possol = (-b+sqrt(b^2-4ac))/2
		final Division possol = new Division(coeffMinusB.add(coeffE), Int.TWO);
		// negsol = (-b-sqrt(b^2-4ac))/2
		final Division negsol = new Division(coeffMinusB.add(Int.MINUSONE.multiply(coeffE)), Int.TWO);
		final Letter letter = poly.getLetter();
		if (kind == EQUALS) {
			// pos = x=(-b+sqrt(b^2-4ac))/2
			final Expression pos = new BooleanOp(letter, EQUALS, possol);
			// neg = x=(-b-sqrt(b^2-4ac))/2
			final Expression neg = new BooleanOp(letter, EQUALS, negsol);

			final ExpressionTree ptree = leaf.addChild(pos);
			appendStepSimplifyChain(ptree);

			if (!pos.toCleanString().equals(neg.toCleanString())) {
				ExpressionTree brother = ptree.addBrother(neg);
				appendStepSimplifyChain(brother);
			}
		} else {
			if (coeffA instanceof IReal) {
				IReal ra = (IReal) coeffA;
				boolean isLeftKind = kind == Operation.LESS || kind == Operation.LEQ;

				final char leftKind;
				if (isLeftKind)
					leftKind = kind;
				else
					leftKind = invert(kind);

				boolean outer = ra.isPositive() ^ isLeftKind;
				if (outer) {
					final char rightKind = invert(leftKind);
					final Expression neg = new BooleanOp(letter, leftKind, negsol);
					final Expression pos = new BooleanOp(letter, rightKind, possol);

					final ExpressionTree ptree = leaf.addChild(pos);
					appendStepSimplifyChain(ptree);

					ExpressionTree brother = ptree.addBrother(neg);
					appendStepSimplifyChain(brother);
				} else {
					final Expression left = new BooleanOp(negsol, leftKind, letter);
					final Expression right = new BooleanOp(letter, leftKind, possol);
					final ExpressionTree ptree = leaf.addChild(new Expression[] { left, right });
					appendStepSimplifyChain(ptree);
				}
			}
		}
	}

	private static ExpressionTree poly2addfirstep(final ExpressionTree sim, final Poly poly, final Expression coeffA,
			final Expression coeffB, final Expression coeffC, char kind) {
		final Letter lvar = poly.getLetter();
		final Expression varsquared = SimplificationHelper.simplify(lvar.square());
		final Expression firstep = new BooleanOp(coeffA.multiply(varsquared).add(coeffB.multiply(lvar)).add(coeffC),
				kind, Int.ZERO);
		return sim.addChild(firstep);
	}

	private static void poly2zerob(final ExpressionTree sim, final Poly poly, final char kind, final Expression coeffA,
			final Expression coeffC) {
		final Expression xsquared = SimplificationHelper.simplify(poly.getLetter().square());
		final Expression firstep = new BooleanOp(coeffA.multiply(xsquared).add(coeffC), kind, Int.ZERO);
		final Expression secstep = SimplificationHelper
				.simplify(new BooleanOp(coeffA.multiply(xsquared), kind, coeffC.opposite()));
		final ExpressionTree firstLeaf = sim.addChild(firstep);
		final ExpressionTree secLeaf = firstLeaf.addChild(secstep);
		final char nkind = getNewKind(kind, coeffA);
		final Expression sol = SimplificationHelper.simplify(new Sqrt(coeffC.opposite().divide(coeffA)));
		final Expression pos;
		final Expression neg;
		final Expression minussol = SimplificationHelper.simplify(sol.opposite());
		final BooleanOp standard = new BooleanOp(poly.getLetter(), nkind, sol);
		switch (nkind) {
		case EQUALS:
			pos = standard;
			neg = new BooleanOp(poly.getLetter(), nkind, minussol);
			break;
		case Operation.GEQ:
			pos = standard;
			neg = new BooleanOp(poly.getLetter(), Operation.LEQ, minussol);
			break;
		case Operation.GREATER:
			pos = standard;
			neg = new BooleanOp(poly.getLetter(), Operation.LESS, minussol);
			break;
		case Operation.LEQ:
		case Operation.LESS:
			final Expression min = new BooleanOp(minussol, nkind, poly.getLetter());
			secLeaf.addChild(new Expression[] { min, standard });
			return;
		default:
			pos = new BooleanOp(poly.getLetter(), Operation.UNKNOWN, sol);
			neg = null;
			break;
		}
		final ExpressionTree ptree = secLeaf.addChild(pos);
		if (neg != null && !pos.toCleanString().equals(neg.toCleanString()))
			ptree.addBrother(neg);
	}

	protected void solveAbsZero(char evar, Abs abs, char kind, ExpressionTree toret) {
		switch (kind) {
		case Operation.EQUALS:
			super.solveAbsZero(evar, abs, kind, toret);
			return;
		case Operation.GEQ:
			toret.addChild(Letter.FORALL);
			return;
		case Operation.GREATER:
		case Operation.NEQ:
			BooleanOp blast = new BooleanOp(abs.getArgument(), Operation.NEQ, Int.ZERO);
			ExpressionTree child = toret.addChild(blast);
			explicate(child, blast, evar);
			return;
		case Operation.LEQ:
			BooleanOp lblast = new BooleanOp(abs.getArgument(), Operation.EQUALS, Int.ZERO);
			ExpressionTree lchild = toret.addChild(lblast);
			explicate(lchild, lblast, evar);
			return;
		case Operation.LESS:
			toret.addChild(Letter.NOTEXISTS);
			return;
		default:
			toret.addChild(Letter.UNKNOWN);
			break;
		}
	}

	protected void solveAbs(char evar, Abs abs, char kind, Expression right, ExpressionTree toret) {
		if (right instanceof IReal) {
			solveAbsReal(evar, abs, kind, (IReal) right, toret);
			return;
		}
		toret.addChild(Letter.UNKNOWN);
	}

	private void solveAbsReal(char evar, Abs abs, char kind, IReal right, ExpressionTree toret) {
		if (right.isNegative()) {
			switch (kind) {
			case Operation.LESS:
			case Operation.LEQ:
			case Operation.EQUALS:
				toret.addChild(Letter.NOTEXISTS);
				return;
			case Operation.GREATER:
			case Operation.GEQ:
			case Operation.NEQ:
				toret.addChild(Letter.FORALL);
				return;
			default:
				toret.addChild(Letter.UNKNOWN);
				return;
			}
		}
		if (right.isZero()) {
			solveAbsZero(evar, abs, kind, toret);
			return;
		}

		Letter letter = new Letter(evar);

		switch (kind) {
		case Operation.LESS:
		case Operation.LEQ:
			final Expression leftop = new BooleanOp(right.inopposite(), kind, letter);
			final Expression rightop = new BooleanOp(letter, kind, right);
			final ExpressionTree ptree = toret.addChild(new Expression[] { leftop, rightop });
			appendStepSimplifyChain(ptree);
			return;
		case Operation.GREATER:
		case Operation.GEQ:
			final Expression neg = new BooleanOp(letter, invert(kind), right.inopposite());
			final Expression pos = new BooleanOp(letter, kind, right);

			final ExpressionTree gptree = toret.addChild(pos);
			appendStepSimplifyChain(gptree);

			ExpressionTree brother = gptree.addBrother(neg);
			appendStepSimplifyChain(brother);
			return;
		case Operation.EQUALS:
			final Expression eneg = new BooleanOp(letter, kind, right.inopposite());
			final Expression epos = new BooleanOp(letter, kind, right);

			final ExpressionTree eptree = toret.addChild(epos);
			appendStepSimplifyChain(eptree);

			ExpressionTree ebrother = eptree.addBrother(eneg);
			appendStepSimplifyChain(ebrother);
			return;
		case Operation.NEQ:
			final Expression nleftop = new BooleanOp(letter, kind, right.inopposite());
			final Expression nrightop = new BooleanOp(letter, kind, right);
			final ExpressionTree nptree = toret.addChild(new Expression[] { nleftop, nrightop });
			appendStepSimplifyChain(nptree);
			return;
		default:
			toret.addChild(Letter.UNKNOWN);
			break;
		}
	}
}
