package meplot.solver;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.other.InvisibleHold;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Power;
import meplot.expressions.other.Poly;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public final class Solver extends AbstractSolver{
	protected boolean canSolve(final char bool){
		return EQUALS == bool;
	}

	protected void poly1(final ExpressionTree sim, final Poly poly, final char kind){
		final Expression coeffA = poly.getCoefficent(1);
		final Expression coeffB = poly.getCoefficent(0);
		final Expression coeffAX = SimplificationHelper.simplify(coeffA.multiply(poly.getLetter()));
		final Expression firstep = new BooleanOp(coeffAX.add(coeffB), EQUALS, Int.ZERO);
		final Expression minusb = SimplificationHelper.simplify(coeffB.opposite());
		final Expression secstep = new BooleanOp(coeffAX, EQUALS, minusb);
		final ExpressionTree firstleaf = sim.addChild(firstep);
		final ExpressionTree secleaf = firstleaf.addChild(secstep);
		if(coeffA.isZero()){
			if(coeffB.isZero())
				secleaf.addChild(Letter.FORALL);
			else
				secleaf.addChild(Letter.NOTEXISTS);
			return;
		}
		final Expression res1 = SimplificationHelper.simplify(minusb.divide(coeffA));
		secleaf.addChild(new BooleanOp(poly.getLetter(), EQUALS, res1));
	}

	protected void poly2(final ExpressionTree sim, final Poly poly, final char kind){
		final Expression coeffA = poly.getCoefficent(2);
		final Expression coeffB = poly.getCoefficent(1);
		final Expression coeffC = poly.getCoefficent(0);
		if(coeffB.isZero()){
			poly2zerob(sim, poly, coeffA, coeffC);
			return;
		}

		// ax^2+bx+c = 0
		final ExpressionTree leaf = poly2addfirstep(sim, poly, coeffA, coeffB, coeffC);

		// mqac = -4ac
		final Expression mqac = Int.FOUR.multiply(coeffA).multiply(coeffC).opposite();
		// d = b^2-4ac
		final Expression coeffD = new Power(coeffB, Int.TWO).add(mqac);
		// e = sqrt(b^2-4ac)
		final Expression coeffE = new Sqrt(coeffD);
		// mb = -b
		final ICalculable coeffMinusB = coeffB.opposite();
		// twoA = 2a
		Expression twoA = Int.TWO.multiply(coeffA);
		// possol = (-b+sqrt(b^2-4ac))/2a
		final Division possol = new Division(coeffMinusB.add(coeffE), twoA);
		// pos = x=(-b+sqrt(b^2-4ac))/2a
		final Expression pos = new BooleanOp(poly.getLetter(), EQUALS, possol);
		// negsol = (-b-sqrt(b^2-4ac))/2a
		final Division negsol = new Division(coeffMinusB.add(Int.MINUSONE.multiply(coeffE)), new InvisibleHold(twoA));
		// neg = x=(-b-sqrt(b^2-4ac))/2a
		final Expression neg = new BooleanOp(poly.getLetter(), EQUALS, negsol);

		final ExpressionTree ptree = leaf.addChild(pos);

		if(!pos.toCleanString().equals(neg.toCleanString()))
			ptree.addBrother(neg);
	}

	private static ExpressionTree poly2addfirstep(final ExpressionTree sim, final Poly poly, final Expression coeffA,
			final Expression coeffB, final Expression coeffC){
		final Letter lvar = poly.getLetter();
		final Expression varsquared = SimplificationHelper.simplify(lvar.square());
		final Expression firstep = new BooleanOp(coeffA.multiply(varsquared).add(coeffB.multiply(lvar)).add(coeffC),
				EQUALS, Int.ZERO);
		return sim.addChild(firstep);
	}

	private static void poly2zerob(final ExpressionTree sim, final Poly poly, final Expression coeffA,
			final Expression coeffC){
		final Expression xsquared = SimplificationHelper.simplify(poly.getLetter().square());
		final Expression firstep = new BooleanOp(coeffA.multiply(xsquared).add(coeffC), EQUALS, Int.ZERO);
		final Expression secstep = new BooleanOp(coeffA.multiply(xsquared), EQUALS,
				SimplificationHelper.simplify(coeffC.opposite()));
		final Expression sol = SimplificationHelper.simplify(new Sqrt(coeffC.opposite().divide(coeffA)));
		final Expression pos = new BooleanOp(poly.getLetter(), EQUALS, sol);
		final Expression neg = new BooleanOp(poly.getLetter(), EQUALS, SimplificationHelper.simplify(sol.opposite()));
		final ExpressionTree firstLeaf = sim.addChild(firstep);
		final ExpressionTree secLeaf = firstLeaf.addChild(secstep);

		final ExpressionTree ptree = secLeaf.addChild(pos);

		if(!pos.toCleanString().equals(neg.toCleanString()))
			ptree.addBrother(neg);
	}
}
