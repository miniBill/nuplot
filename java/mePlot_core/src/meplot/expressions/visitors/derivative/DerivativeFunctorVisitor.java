package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.exceptions.DerivationException;
import meplot.expressions.functions.ExpandFunction;
import meplot.expressions.functions.MonicFunction;
import meplot.expressions.functions.algebra.Span;
import meplot.expressions.functions.matrix.MatrixFunction;
import meplot.expressions.functions.ode.GenericOde;
import meplot.expressions.functions.operations.Integral;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;
import meplot.parser.tokens.FakeFunction;

public class DerivativeFunctorVisitor implements IExpressionFunctorVisitor{
	private final IExpressionMonicFunctionVisitor dmonv;
	private final IExpressionNonsymbolicFunctionVisitor dnsyv;
	private final DerivativeVisitor parent;

	public DerivativeFunctorVisitor(final DerivativeVisitor parent){
		this.parent = parent;
		dmonv = new DerivativeMonicFunctionVisitor(parent);
		dnsyv = new DerivativeNonsymbolicFunctionVisitor(parent);
	}

	public Expression visit(final ExpandFunction expandFunction){
		return parent.genvisit(expandFunction.expand());
	}

	public Expression visit(final FakeFunction fakeFunction){
		throw new DerivationException("Fakefunction derivative");
	}

	public Expression visit(final GenericOde genericOde){
		return Int.ZERO;
	}

	public Expression visit(final Integral integral){
		final Expression[] iargs = integral.getArguments();

		final Expression expr = iargs[0];
		final Expression dvarexpr = iargs[1];
		final Expression loexpr = iargs[2];
		final Expression hiexpr = iargs[3];

		final char dvar = dvarexpr.toString().charAt(0);

		final Expression hider = parent.genvisit(hiexpr);
		final ICalculable high = expr.partialSubstitute(dvar, hiexpr).multiply(hider);

		final Expression loder = parent.genvisit(loexpr);
		final ICalculable low = expr.partialSubstitute(dvar, loexpr).multiply(loder);

		return high.add(low.opposite());
	}

	public Expression visit(final MatrixFunction matrixFunction){
		final Expression sim = matrixFunction.partialSimplify();
		if(matrixFunction.isIdentical(sim))
			throw new DerivationException();
		return parent.genvisit(sim);
	}

	public Expression visit(final MonicFunction monicFunction){
		return monicFunction.accept(dmonv);
	}

	public Expression visit(final NonsymbolicFunction nonsymbolicFunction){
		return nonsymbolicFunction.accept(dnsyv);
	}

	public Expression visit(final Span span){
		return Int.ZERO;
	}
}
