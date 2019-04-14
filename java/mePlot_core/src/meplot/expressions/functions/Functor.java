package meplot.expressions.functions;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.visitors.IExpressionVisitor;

/**
 * Generic function, doesn't define any implementation details.
 * 
 * @author Leonardo Taglialegne
 */
public abstract class Functor extends AbstractExpression implements IFunctor{
	/**
	 * {@inheritDoc}
	 */
	public final boolean needParenthesis(){
		return false;
	}

	public final boolean isIdentical(final Expression other){
		if(!(other instanceof IFunctor))
			return false;
		final IFunctor oth = (IFunctor)other;
		if(!getName().equals(oth.getName()))
			return false;
		return areIdentical(getArguments(), oth.getArguments());
	}

	public final Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuilder buffer){
		buffer.append(getName());
		buffer.append('(');
		final Expression[] args = getArguments();
		for(int i = 0; i < args.length; i++){
			args[i].toWrappedHtml(buffer);
			if(i < args.length - 1)
				buffer.append(',');
		}
		buffer.append(')');
	}

	public final boolean toStringStartsWith(char prefix){
		return getName().indexOf(prefix) == 0;
	}
}
