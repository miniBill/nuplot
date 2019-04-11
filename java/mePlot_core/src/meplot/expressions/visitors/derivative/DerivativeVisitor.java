package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.functions.IFunctor;
import meplot.expressions.functions.exp.Ln;
import meplot.expressions.functions.piecewise.IPower;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.*;
import meplot.expressions.other.Proxy;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.expressions.visitors.IExpressionTensorVisitor;
import meplot.expressions.visitors.IExpressionVisitor;
import platform.lists.IIterable;

import java.util.Iterator;

public class DerivativeVisitor implements IExpressionVisitor {
	private final IExpressionFunctorVisitor dfunv;
	private final IExpressionTensorVisitor dtenv;
	private final char var;

	public DerivativeVisitor(final char var) {
		this.var = var;
		dfunv = getFunctorVisitor();
		dtenv = new DerivativeTensorVisitor(this);
	}

	protected Expression genvisit(final Expression expr) {
		return expr.accept(this);
	}

	protected IExpressionFunctorVisitor getFunctorVisitor() {
		return new DerivativeFunctorVisitor(this);
	}

	public char getVariable() {
		return var;
	}

	public Expression visit(final BooleanOp booleanOp) {
		return Int.ZERO;
	}

	public Expression visit(final IDivision division) {
		final Expression numerator = division.getNumerator();
		final Expression denominator = division.getDenominator();
		final ICalculable f1xg = genvisit(numerator).multiply(denominator);
		final ICalculable fxg1 = numerator.multiply(genvisit(denominator));
		final ICalculable newNumerator = f1xg.add(fxg1.opposite());
		final Expression newDenominator = denominator.square();
		return newNumerator.divide(newDenominator);
	}

	public Expression visit(final IFunctor functor) {
		return functor.accept(dfunv);
	}

	public Expression visit(final IMultiplication multiplication) {
        if (IIterable.isEmpty(multiplication))
			return Int.ZERO;
		final Iterator<Expression> factors = multiplication.iterator();
		final Expression first = factors.next();
		if (!factors.hasNext())
			return genvisit(first);
		final Expression others = new Multiplication(factors);
		final ICalculable firstD = genvisit(first);
		final ICalculable left = firstD.multiply(others);
		final Expression othersD = genvisit(others);
		final Expression right = first.multiply(othersD);
		return left.add(right);
	}

	public Expression visit(final IPower power) {
		final Expression base = power.getBase();
		final Expression exponent = power.getExponent();
		// g(x)f(x)^(g(x)-1)f'(x)+f(x)^(g(x))log(f(x))g'(x);
		final ICalculable dbase = genvisit(base);
		final ICalculable dexp = genvisit(exponent);
		final Expression loga = new Ln(base);
		final Expression dexpm1 = new Sum(exponent, new Int(-1));
		final ICalculable left = dbase.multiply(exponent).multiply(new Power(base, dexpm1));
		final Expression right = dexp.multiply(loga).multiply(new Power(base, exponent));
		return left.add(right);
	}

	public Expression visit(final ITensor tensor) {
		return tensor.accept(dtenv);
	}

	public Expression visit(final Lambda lambda) {
		return new Lambda(lambda.getLeft(), genvisit(lambda.getRight()));
	}

	public Expression visit(final Letter letter) {
		if (letter.getLetter() == var)
			return Int.ONE;
		return Int.ZERO;
	}

	public Expression visit(final Proxy proxy) {
		return genvisit(proxy.getValue());
	}

	public Expression visit(final Sum sum) {
		final Iterator<Expression> addends = sum.iterator();
		if (IIterable.isSingle(sum))
			return genvisit(addends.next());
		Expression toret = Int.ZERO;
		while (addends.hasNext())
			toret = toret.add(genvisit(addends.next()));
		return toret;
	}
}
