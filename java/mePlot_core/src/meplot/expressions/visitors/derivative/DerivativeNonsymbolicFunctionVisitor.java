package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.functions.operations.Mod;
import meplot.expressions.functions.other.Ackermann;
import meplot.expressions.functions.other.Gcd;
import meplot.expressions.functions.other.Mandelbrot;
import meplot.expressions.functions.piecewise.Max;
import meplot.expressions.functions.piecewise.Min;
import meplot.expressions.functions.piecewise.Piecewise;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Operation;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public class DerivativeNonsymbolicFunctionVisitor implements IExpressionNonsymbolicFunctionVisitor{
	private final DerivativeVisitor parent;

	public DerivativeNonsymbolicFunctionVisitor(final DerivativeVisitor parent){
		this.parent = parent;
	}

	public Expression visit(final Ackermann ackermann){
		return Int.ZERO;
	}

	public Expression visit(final Gcd gcd){
		return Int.ZERO;
	}

	public Expression visit(final Mandelbrot mandelbrot){
		return Int.ZERO;
	}

	public Expression visit(final Max max){
		final Expression left = max.getArguments()[0];
		final Expression dleft = parent.genvisit(left);
		final Expression right = max.getArguments()[1];
		final Expression dright = parent.genvisit(right);
		return new Piecewise(new BooleanOp(left, Operation.GREATER, right), dleft, dright);
	}

	public Expression visit(final Min min){
		final Expression left = min.getArguments()[0];
		final Expression dleft = parent.genvisit(left);
		final Expression right = min.getArguments()[1];
		final Expression dright = parent.genvisit(right);
		return new Piecewise(new BooleanOp(left, Operation.LESS, right), dleft, dright);
	}

	public Expression visit(final Mod mod){
		return parent.genvisit(mod.getArguments()[0]);
	}

	public Expression visit(final Piecewise piecewise){
		return new Piecewise(piecewise.getCondition(), parent.genvisit(piecewise.getIf()), parent.genvisit(piecewise
				.getIfnot()));
	}
}
