package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.exceptions.DerivationException;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;
import meplot.expressions.visitors.IVisitable;

public final class DerivativeHelper{
	private static final IExpressionVisitor xsvis = new StepDerivativeVisitor('x');
	private static final IExpressionVisitor xvis = new DerivativeVisitor('x');

	public static Expression derivative(final IVisitable expression, final char var){
		if(var == 'x')
			return expression.accept(xvis);
		return expression.accept(new DerivativeVisitor(var));
	}

	public static Expression derivativeOrDefault(final IVisitable expression, final char var){
		try{
			return derivative(expression, var);
		}
		catch(final DerivationException e){
			return Int.ZERO;
		}
	}

	public static Expression stepDerivative(final IVisitable expression, final char var){
		if(var == 'x')
			return expression.accept(xsvis);
		return expression.accept(new StepDerivativeVisitor(var));
	}

	public static Expression stepDerivativeOrDefault(final IVisitable expression, final char var){
		try{
			return stepDerivative(expression, var);
		}
		catch(final DerivationException e){
			return Int.ZERO;
		}
	}

	private DerivativeHelper(){
	}
}
