package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.DerivationException;
import meplot.expressions.functions.operations.Derivative;
import meplot.expressions.functions.piecewise.IPower;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.parser.tokens.FakeFunction;

public class StepDerivativeVisitor extends DerivativeVisitor{

	public StepDerivativeVisitor(final char var){
		super(var);
	}

	protected final Expression genvisit(final Expression expr){
		return new Derivative(expr, getVariable());
	}

	protected IExpressionFunctorVisitor getFunctorVisitor(){
		return new DerivativeFunctorVisitor(this){
			public Expression visit(final FakeFunction fakeFunction){
				throw new DerivationException("Fakefunction stepDerivative");
			}
		};
	}

	public Expression visit(final IPower power){
		final Expression exponent = power.getExponent();
		if(exponent instanceof INumber){
			final Expression base = power.getBase();
			final char variable = getVariable();
			if(base instanceof Letter){
				if(((Letter)base).getLetter() == variable){
					final Power mid = new Power(base, exponent.add(Int.MINUSONE));
					return exponent.multiply(mid);
				}
				return Int.ZERO;
			}
			final Power mid = new Power(base, exponent.add(Int.MINUSONE));
			return exponent.multiply(mid).multiply(new Derivative(base, variable));
		}

		return super.visit(power);
	}
}
