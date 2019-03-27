package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.functions.operations.Mod;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IInt;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Division;
import meplot.expressions.operations.Lambda;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.Power;
import meplot.expressions.operations.Sum;
import meplot.parser.ParserException;

public final class OperationToken extends Token{
	private static final String DOT = ".";
	private final char val;

	public OperationToken(final char value){
		val = value;
	}

	public IToken getRight(){
		return right;
	}

	public IToken getLeft(){
		return left;
	}

	private IToken left;
	private IToken right;

	public void setRight(final IToken value){
		right = value;
	}

	public void setLeft(final IToken value){
		left = value;
	}

	public String toString(){
		final StringBuffer toret = new StringBuffer();
		toString(toret);
		return toret.toString();
	}

	public void toString(final StringBuffer toret){
		final boolean bothNotNull = left != null && right != null;
		if(bothNotNull)
			toret.append('(');
		toStringOrDOT(toret, left);
		toret.append(val);
		toStringOrDOT(toret, right);
		if(bothNotNull)
			toret.append(')');
	}

	private static void toStringOrDOT(final StringBuffer toret, final IToken value){
		if(value == null)
			toret.append(DOT);
		else
			value.toString(toret);
	}

	public Expression toExpression() throws ParserException{
		if(left == null || right == null)
			throw new ParserException();
		final Expression lefte = left.toExpression();
		final Expression righte = right.toExpression();
		switch(val){
			case Operation.ADDITION:
				return new Sum(lefte, righte);
			case Operation.MULTIPLICATION:
				return new Multiplication(lefte, righte);
			case Operation.DIVISION:
				if(lefte instanceof IInt && righte instanceof IInt)
					return new Fraction((IInt)lefte, (IInt)righte);
				return new Division(lefte, righte);
			case Operation.POWER:
				return new Power(lefte, righte);
			case Operation.MOD:
				return new Mod(lefte, righte);
			case Operation.LAMBDA:
				return new Lambda(lefte, righte);
			default:
				return new BooleanOp(lefte, val, righte);
		}
	}

	public char getVal(){
		return val;
	}
}
