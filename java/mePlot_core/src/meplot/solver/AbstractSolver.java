package meplot.solver;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterable;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Operation;
import meplot.expressions.other.Poly;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.solver.states.HeadSolverState;
import meplot.solver.states.ISystemSolverState;

public abstract class AbstractSolver implements ISolver {
	protected static final char EQUALS = '=';

	/**
	 * Activates the [experimental] cross simplification in Division.
	 * 
	 * @return Previous value for the setting.
	 */
	public static boolean activateCross() {
		Division.activateCross();
		return true;
	}

	protected static ExpressionTree appendStepSimplifyChain(final ExpressionTree tree) {
		ExpressionTree leaf = tree;
		final IExpressionIterable root = tree.getValue();
		final IIterator<Expression>[] chains = new IIterator[root.length()];
		final IIterator<Expression> iterator = root.getIterator();
		for (int i = 0; i < chains.length; i++)
			chains[i] = SimplificationHelper.stepSimplify(iterator.next()).getIterator();
		while (true) {
			final ExpressionList current = new ExpressionList();
			boolean done = true;
			for (IIterator<Expression> chain : chains)
				if (chain.length() > 1) {
					current.add(chain.next());
					done = false;
				} else
					current.add(chain.getCurrent());
			leaf = leaf.addChild(current);
			if (done)
				break;
		}
		final IExpressionList last = new ExpressionList();
		for (IIterator<Expression> chain : chains) last.add(SimplificationHelper.simplify(chain.next()));
		return leaf.addChild(last);
	}

	private static Expression craftEquivalentEquation(final BooleanOp feq) {
		if (feq.getLeft() instanceof Abs)
			return feq;
		if (feq.getLeft() instanceof Sqrt)
			return feq;
		final Expression equivalent;
		if (feq.getRight().isZero())
			equivalent = feq;
		else
			equivalent = new BooleanOp(feq.getLeft().add(feq.getRight().opposite()), feq.getBool(), Int.ZERO);
		return equivalent;
	}

	public static void deactivateCross() {
		Division.deactivateCross();
	}

	private static char getFirstVar(final Expression expr) {
		char firstVar = 'x';
		while (firstVar > 'a' && expr.hasLetter((char) (firstVar - 1)))
			firstVar--;
		if (firstVar == 'x' && !expr.hasLetter('x')) {
			if (expr.hasLetter('y'))
				return 'y';
			if (expr.hasLetter('z'))
				return 'z';
			while (firstVar >= 'a' && !expr.hasLetter(firstVar))
				firstVar--;
			if (firstVar == 'a' - 1)
				return 'x';
			return firstVar;
		}
		return firstVar;
	}

	public static char getFirstVar(final IExpressionIterable equations) {
		char firstVar = 'x';
		while (firstVar > 'a' && hasLetter(equations, (char) (firstVar - 1)))
			firstVar--;
		if (firstVar == 'x' && !hasLetter(equations, 'x')) {
			if (hasLetter(equations, 'y'))
				return 'y';
			if (hasLetter(equations, 'z'))
				return 'z';
			while (firstVar >= 'a' && !hasLetter(equations, firstVar))
				firstVar--;
			if (firstVar == 'a' - 1)
				return 'x';
			return firstVar;
		}
		return firstVar;
	}

	private static Expression getGcdForPoly(final Poly poly) {
		final int deg = poly.getDegree();
		IInt toret = Int.ONE;
		for (int i = 1; i < deg; i++) {
			final Expression coeff = poly.getCoefficent(i);
			if (coeff instanceof Fraction) {
				final Fraction frac = (Fraction) coeff;
				toret = FunctionsMath.lcm(toret, frac.fgetDenominator());
			}
		}
		return toret;
	}

	private static boolean hasLetter(final IExpressionIterable equations, final char letter) {
		for (Expression curr : equations)
			if (curr.hasLetter(letter))
				return true;
		return false;
	}

	protected abstract boolean canSolve(final char kind);

	public ExpressionTree explicate(final Expression last, final char var) {
		final ExpressionTree toret = new ExpressionTree(last);
		explicate(toret, var);
		return toret;
	}

	protected final void explicate(final ExpressionTree toret, final BooleanOp blast, final char var) {
		final Expression left = blast.getLeft();
		final Expression right = blast.getRight();
		final char kind = blast.getBool();
		final char evar;
		if (blast.hasLetter(var))
			evar = var;
		else
			evar = getFirstVar(blast);
		if (left instanceof Letter && ((Letter) left).getLetter() == evar && !right.hasLetter(evar))
			return;
		if (right.isZero()) {
			if (Poly.isPoly(left, evar)) {
				final ExpressionTree polysolution = polySolve(new Poly(left, evar), blast.getBool());
				toret.addChild(polysolution);
				return;
			}
			if (left instanceof Division) {
				final Division dleft = (Division) left;
				if (EQUALS == kind) {
					final BooleanOp op = new BooleanOp(dleft.getNumerator(), kind, Int.ZERO);
					final ExpressionTree child = toret.addChild(op);
					explicate(child, op, evar);
					return;
				}
			}
			if (left instanceof Matrix) {
				final IIterator<Expression> elements = ((Matrix) left).getElements();
				final IExpressionList list = new ExpressionList();
				while (elements.hasNext())
					list.add(new BooleanOp(elements.next(), Operation.EQUALS, Int.ZERO));
				toret.addChild(list);
				return;
			}
			if (left instanceof Abs) {
				final Abs abs = (Abs) left;
				solveAbsZero(evar, abs, kind, toret);
				return;
			}
			if (left instanceof Sqrt) {
				final Sqrt sqrt = (Sqrt) left;
				solveSqrtZero(evar, sqrt, kind, toret);
				return;
			}
		} else {
			if (left instanceof Abs) {
				Abs abs = (Abs) left;
				solveAbs(evar, abs, kind, right, toret);
				return;
			}
			if (left instanceof Sqrt) {
				Sqrt sqrt = (Sqrt) left;
				solveSqrt(evar, sqrt, kind, right, toret);
				return;
			}
			final Expression leftMinusRight = left.add(right.opposite());
			final BooleanOp equiv = new BooleanOp(leftMinusRight, kind, Int.ZERO);
			final ExpressionTree child = toret.addChild(equiv);
			final ExpressionTree lastChild = appendStepSimplifyChain(child);
			explicate(lastChild, evar);
			return;
		}
		if (!blast.isSimplified()) {
			final Expression simplified = SimplificationHelper.simplify(blast);
			if (!simplified.isSimplified())
				throw new CalcException("Like lol wtf in explicate, " + simplified.toFullString());
			final ExpressionTree child = toret.addChild(simplified);
			explicate(child, evar);
			return;
		}
		final Expression expanded = blast.expand();
		if (!blast.isIdentical(expanded)) {
			final ExpressionTree child = toret.addChild(expanded);
			explicate(child, evar);
			return;
		}
		throw new CalcException("Failed to explicate wrt " + evar + ", left was " + left + ", right was "
				+ right);
	}

	private void explicate(final ExpressionTree toret, final char var) {
		final Expression last = toret.getValue().getFirst();
		if (last instanceof BooleanOp) {
			final BooleanOp blast = (BooleanOp) last;
			explicate(toret, blast, var);
			return;
		} else if (last.equals(Letter.FORALL) || last.equals(Letter.NOTEXISTS) || last.equals(Letter.UNKNOWN))
			return;
		throw new CalcException("Failed to explicate, last was " + last);
	}

	private Expression getEquivalent(final Expression equation) {
		if (equation instanceof INumber)
			return equation;
		if (equation instanceof BooleanOp) {
			final BooleanOp feq = (BooleanOp) equation;
			if (canSolve(feq.getBool()))
				return craftEquivalentEquation(feq);
			return feq;
		}
		if (equation instanceof Matrix)
			return equation;
		return new BooleanOp(equation, EQUALS, Int.ZERO);
	}

	protected abstract void poly1(ExpressionTree sim, Poly poly, char kind);

	protected abstract void poly2(ExpressionTree sim, Poly poly, char kind);

	private ExpressionTree polySolve(final Poly poly, final char kind) {
		final Poly poly2 = poly.sumExpand();
		if (!poly.isIdentical(poly2))
			return polySolve(poly2, kind);
		final ExpressionTree toret = new ExpressionTree(new BooleanOp(poly, kind, Int.ZERO));
		final Expression gcd = getGcdForPoly(poly);
		if (!gcd.isOne()) {
			final Expression newPoly = SimplificationHelper.simplify(poly.multiply(gcd));
			toret.addChild(new BooleanOp(newPoly, kind, Int.ZERO));
			return toret;
		}
		final int degree = poly.getDegree();
		switch (degree) {
		case 0:
			if (poly.isZero())
				toret.addChild(Letter.FORALL);
			else
				toret.addChild(Letter.NOTEXISTS);
			break;
		case 1:
			poly1(toret, poly, kind);
			break;
		case 2:
			poly2(toret, poly, kind);
			break;
		default:
			toret.addChild(Letter.UNKNOWN);
			break;
		}
		return toret;
	}

	public final Solution solve(final Expression equation) {
		char firstVar = getFirstVar(equation);
		return solve(equation, firstVar);
	}

	public final Solution solve(final Expression equation, final char var) {
		final boolean old = activateCross();
		final Expression equivalent = getEquivalent(equation);

		final ExpressionTree root = new ExpressionTree(equation);
		final ExpressionTree child = root.addChild(equivalent);
		final ExpressionTree leave = appendStepSimplifyChain(child);

		systemSolve(leave);
		if (old)
			deactivateCross();
		return new Solution(root);
	}

	protected void solveAbs(final char evar, Abs abs, final char kind, final Expression right,
			final ExpressionTree toret) {
		if (EQUALS == kind) {
			final Expression arg = abs.getArgument();
			final BooleanOp first = new BooleanOp(arg, EQUALS, right);
			final BooleanOp second = new BooleanOp(arg, EQUALS, right.opposite());
			final ExpressionTree child = toret.addChild(first);
			final ExpressionTree brother = child.addBrother(second);
			explicate(child, first, evar);
			explicate(brother, second, evar);
		} else {
			toret.addChild(Letter.UNKNOWN);
		}
	}

	protected void solveAbsZero(final char evar, final Abs abs, final char kind, final ExpressionTree toret) {
		if (EQUALS == kind) {
			final BooleanOp op = new BooleanOp(abs.getArgument(), EQUALS, Int.ZERO);
			final ExpressionTree child = toret.addChild(op);
			explicate(child, op, evar);
		} else
			toret.addChild(Letter.UNKNOWN);
	}

	private void solveSqrt(final char evar, Sqrt sqrt, final char kind, final Expression right,
			final ExpressionTree toret) {
		if (EQUALS == kind) {
			final Expression arg = sqrt.getArgument();
			final BooleanOp check = new BooleanOp(right, Operation.GEQ, Int.ZERO);
			final BooleanOp squared = new BooleanOp(arg, EQUALS, right.square());
			final ExpressionTree child = toret.addChild(new Expression[] { squared, check });
			explicate(child, squared, evar);
		} else {
			toret.addChild(Letter.UNKNOWN);
		}
	}

	private void solveSqrtZero(final char evar, final Sqrt sqrt, final char kind, final ExpressionTree toret) {
		if (EQUALS == kind) {
			final BooleanOp op = new BooleanOp(sqrt.getArgument(), EQUALS, Int.ZERO);
			final ExpressionTree child = toret.addChild(op);
			explicate(child, op, evar);
		} else
			toret.addChild(Letter.UNKNOWN);
	}

	private void systemSolve(final ExpressionTree sim) {
		final ISystemSolverState head = new HeadSolverState(sim, this);
		head.solve();
	}
}
