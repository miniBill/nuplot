package meplot.expressions.functions.matrix;

import meplot.expressions.Expression;
import meplot.expressions.functions.Functor;
import meplot.expressions.functions.IFunctor;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionFunctorVisitor;

public abstract class MatrixFunction extends Functor{
	private final Expression mat;

	protected MatrixFunction(final Expression expr){
		mat = expr;
	}

	public final Expression[] getArguments(){
		// TODO: is this optimal?
		return new Expression[]{mat};
	}

	public final boolean containsMatrix(){
		return true;
	}

	public final double dvalue(final char letter, final double value){
		if(mat instanceof Matrix)
			return value((Matrix)mat).dvalue(letter, value);
		return 0;
	}

	public abstract MatrixFunction fill(final Expression expr);

	public final IFunctor gfill(final Expression[] args){
		return fill(args[0]);
	}

	public abstract String getName();

	public final boolean hasLetter(final char letter){
		return mat.hasLetter(letter);
	}

	public final Expression innerSimplify(){
		if(mat instanceof Matrix)
			return value((Matrix)mat);
		if(mat.isSimplified())
			return this;
		final Expression part = mat.partialSimplify();
		return fill(part);
	}

	public final Expression innerStepSimplify(){
		if(mat instanceof Matrix && mat.isSimplified())
			return value((Matrix)mat);
		if(mat.isSimplified())
			return this;
		final Expression part = mat.innerStepSimplify();
		if(part.equals(mat))
			return value((Matrix)mat);
		return fill(part);
	}

	public final int needs(){
		return 1;
	}

	public final Expression partialSubstitute(final IValueList valueList){
		return fill(mat.partialSubstitute(valueList));
	}

	public final Expression partialSubstitute(final char letter, final double value){
		return fill(mat.partialSubstitute(letter, value));
	}

	public final Expression partialSubstitute(final char letter, final Expression value){
		return fill(mat.partialSubstitute(letter, value));
	}

	public final void toFullString(final StringBuilder buffer){
		buffer.append(getName());
		buffer.append('(');
		mat.toFullString(buffer);
		buffer.append(')');
	}

	public final void toString(final StringBuilder buffer){
		buffer.append(getName());
		buffer.append('(');
		mat.toString(buffer);
		buffer.append(')');
	}

	protected abstract Expression value(final Matrix matrix);

	public final INumber value(final IValueList letters){
		if(isSimplified())
			return Int.ZERO;
		return partialSimplify().value(letters);
	}

	public Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
