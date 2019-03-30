package meplot.graphics.graphs;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.Letter;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.visitors.derivative.DerivativeHelper;

public class NormalGraph extends AbstractGraph{
	private final Expression expr;
	private Expression derivative;
	private Expression dderivative;

	public NormalGraph(final ISubstitutible function, final int color){
		super(color);
		ISubstitutible texpr = function;
		if(texpr instanceof BooleanOp){
			final BooleanOp bexpr = (BooleanOp)texpr;
			if(bexpr.getLeft().equals(Letter.Y) && !bexpr.getRight().hasLetter('y') || bexpr.getLeft().equals(Letter.Z)
					&& !bexpr.getRight().hasLetter('z'))
				texpr = bexpr.getRight();
			else
				if(bexpr.hasLetter('z'))
					texpr = bexpr.getRight().add(bexpr.getLeft().opposite());
		}
		expr = texpr.applyConstants();
	}

	public final boolean isRadial(){
		if(expr == null)
			return false;
		return expr.hasLetter('r');
	}

	public final Expression getExpression(){
		return expr;
	}

	public final Expression getD(){
		if(derivative == null)
			derivative = DerivativeHelper.derivativeOrDefault(expr, 'x').partialSimplify();
		return derivative;
	}

	public final Expression getDD(){
		if(dderivative == null)
			dderivative = DerivativeHelper.derivativeOrDefault(getD(), 'x').partialSimplify();
		return dderivative;
	}

	public boolean is3D(){
		if(expr instanceof BooleanOp){
			if(isYEquals())
				return false;
			return expr.hasLetter('z');
		}
		return expr.hasLetter('y') || expr.hasLetter('z');
	}

	private boolean isYEquals(){
		if(expr instanceof BooleanOp){
			final BooleanOp bexpr = (BooleanOp)expr;
			return bexpr.getLeft().equals(Letter.Y) && !bexpr.getRight().hasLetter('y');
		}
		return false;
	}

	private boolean isZEquals(){
		if(expr instanceof BooleanOp){
			final BooleanOp bexpr = (BooleanOp)expr;
			return bexpr.getLeft().equals(Letter.Z) && !bexpr.getRight().hasLetter('z');
		}
		return false;
	}

	public final boolean isImplicit(){
		return expr instanceof BooleanOp && expr.hasLetter('y') && !isYEquals() || expr.hasLetter('z') && !isZEquals();
	}

	public final boolean isDisequation(){
		return expr instanceof BooleanOp && ((BooleanOp) expr).isDisequation();
	}
}
