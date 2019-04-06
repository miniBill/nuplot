package meplot.expressions.geometry;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.exceptions.CalcException;
import org.jetbrains.annotations.NotNull;
import platform.lists.IIterable;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Operation;
import meplot.expressions.visitors.IExpressionTensorVisitor;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import platform.log.Log;
import platform.log.LogLevel;

import java.util.Iterator;

public class Matrix extends Tensor {
	private static final Matrix ZERO = new Matrix(Int.ZERO);
	private Expression[][] vals;

	public Matrix(final Expression expression) {
		vals = new Expression[1][1];
		vals[0][0] = expression;
	}

	// Build a row matrix
	public Matrix(final Expression[] values) {
		vals = new Expression[1][values.length];
		set(values);
	}

	public Matrix(final Expression[][] values) {
		vals = new Expression[values.length][values.length == 0 ? 0 : values[0].length];
		set(values);
	}

	public Matrix(final IExpressionList exprList, final boolean isVector) {
		if (isVector) {
			fillByList(exprList);
			return;
		}
		if (!(exprList.getFirst() instanceof Matrix))
			return;
		final Matrix firstRow = (Matrix) exprList.getFirst();
		vals = new Expression[exprList.length()][firstRow.vals[0].length];
		final IIterator<Expression> iterator = exprList.getIterator();
		Matrix mat = (Matrix) iterator.next();
		for (int y = 0; y < getRows(); y++) {
			if (getCols() >= 0) System.arraycopy(mat.vals[0], 0, vals[y], 0, getCols());
			if (iterator.hasNext())
				mat = (Matrix) iterator.next();
			else
				break;
		}
	}

	private Matrix(final int dim) {
		vals = new Expression[dim][dim];
	}

	private Matrix(final int rows, final int columns) {
		vals = new Expression[rows][columns];
	}

	public Matrix(final Matrix original) {
		vals = new Expression[original.getRows()][original.getCols()];
		set(original.vals);
	}

	public final ITensor add(final ITensor expr) {
		if (expr instanceof Matrix)
			return add((Matrix) expr);
		if (!expr.containsMatrix() && getRows() == getCols())
			return add(identity(getRows()).multiply(expr));
		throw new CalcException("Don't know how to add " + toFullString() + " to " + expr.toFullString());
	}

	private Matrix add(final Matrix other) {
		if (getRows() != other.getRows() || getCols() != other.getCols()) {
			Log.log(LogLevel.ERROR, "Wrong dimensions in Matrix add");
			return Matrix.ZERO;
		}
		final Matrix toret = new Matrix(getRows(), getCols());
		for (int y = 0; y < getRows(); y++)
			for (int x = 0; x < getCols(); x++)
				toret.vals[y][x] = vals[y][x].add(other.vals[y][x]);
		return toret;
	}

	public final boolean compatible(final Expression elementAt, final char operation) {
		if (elementAt instanceof Matrix && operation != Operation.POWER)
			return true;
		if (operation == Operation.MULTIPLICATION || operation == Operation.DIVISION)
			return !elementAt.containsMatrix();
		return operation == Operation.ADDITION && getRows() == getCols() && !elementAt.containsMatrix();
	}

	public final boolean containsMatrix() {
		return true;
	}

	public final Expression[][] deepCopy() {
		final Expression[][] toret = new Expression[getRows()][getCols()];
		for (int x = 0; x < vals.length; x++)
			System.arraycopy(vals[x], 0, toret[x], 0, vals[0].length);
		return toret;
	}

	public final ITensor divide(final ITensor arg) {
		if (arg instanceof Matrix) {
			final Matrix minverse = ((Matrix) arg).minverse();
			return multiply(minverse);
		} else if (arg instanceof INumber) {
			final Matrix toret = new Matrix(getRows(), getCols());
			for (int y = 0; y < getRows(); y++)
				for (int x = 0; x < getCols(); x++)
					toret.vals[y][x] = vals[y][x].divide(arg);
			return toret;
		}
		throw new CalcException("Couldn't divide " + this.toFullString() + " by " + arg.toFullString());
	}

	private Matrix doMultiply(final Expression expr) {
		final Matrix toret = new Matrix(getRows(), getCols());
		for (int x = 0; x < getCols(); x++)
			for (int y = 0; y < getRows(); y++)
				toret.vals[y][x] = vals[y][x].multiply(expr);
		return toret;
	}

	public final double dvalue(final char letter, final double value) {
		return vals[0][0].dvalue(letter, value);
	}

	public final double fdvalue(final char letter, final double value) {
		return vals[0][0].fdvalue(letter, value);
	}

	private void fillByList(final IIterable<Expression> iterable) {
		vals = new Expression[1][iterable.length()];
		int c = 0;
		for (Expression expression : iterable)
			vals[0][c++] = expression;
	}

	public final Expression gadd(final Expression expr) {
		if (!expr.containsMatrix() && getRows() == getCols()) {
			final Matrix identity = identity(getRows());
			final Matrix mid = identity.doMultiply(expr);
			return add(mid);
		}
		return super.add(expr);
	}

	public final Expression gdivide(final Expression arg) {
		final Matrix toret = new Matrix(getRows(), getCols());
		for (int y = 0; y < getRows(); y++)
			for (int x = 0; x < getCols(); x++)
				toret.vals[y][x] = vals[y][x].divide(arg);
		return toret;
	}

	public final Expression get(final int row, final int col) {
		return vals[row][col];
	}

	public final int getCols() {
		if (vals.length > 0)
			return vals[0].length;
		return 0;
	}

	public final IIterator<Expression> getElements() {
		return new MatrixElementsIterator(this);
	}

	public final int getRows() {
		return vals.length;
	}

	public final Expression gmultiply(final Expression expr) {
		return doMultiply(expr);
	}

	public final Matrix gradient() {
		final Expression[][] toret = new Expression[vals.length][vals[0].length];
		char var = 'x';
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				toret[x][y] = DerivativeHelper.derivative(vals[x][y], var++);
		return new Matrix(toret);
	}

	public final boolean hasLetter(final char letter) {
		for (int x = 0; x < getRows(); x++)
			for (int y = 0; y < getCols(); y++)
				if (vals[x][y].hasLetter(letter))
					return true;
		return false;
	}

	private Matrix identity(final int rows) {
		final Matrix toret = new Matrix(rows);
		for (int y = 0; y < getRows(); y++)
			for (int x = 0; x < getCols(); x++)
				toret.vals[y][x] = x == y ? Int.ONE : Int.ZERO;
		return toret;
	}

	public final Expression innerSimplify() {
		final Expression[][] toret = new Expression[vals.length][vals.length == 0 ? 0 : vals[0].length];
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				toret[x][y] = vals[x][y].partialSimplify();
		return new Matrix(toret);
	}

	public final Expression innerStepSimplify() {
		final Expression[][] toret = new Expression[vals.length][vals[0].length];
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				toret[x][y] = vals[x][y].innerStepSimplify();
		return new Matrix(toret);
	}

	public final boolean isFullDouble() {
		for (Expression[] val : vals)
			for (int y = 0; y < vals[0].length; y++)
				if (!val[y].isFullDouble())
					return false;
		return true;
	}

	public final boolean isIdentical(final Expression other) {
		if (!(other instanceof Matrix))
			return false;
		final Matrix oth = (Matrix) other;
		if (vals.length != oth.vals.length || vals.length > 0 && vals[0].length != oth.vals[0].length)
			return false;
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				if (!vals[x][y].isIdentical(oth.vals[x][y]))
					return false;
		return true;
	}

	public final boolean isOne() {
		if (getRows() != getCols())
			return false;
		for (int x = 0; x < getRows(); x++)
			for (int y = 0; y < getCols(); y++) {
				if (x == y && !vals[x][y].isOne())
					return false;
				if (x != y && !vals[x][y].isZero())
					return false;
			}
		return true;
	}

	public final boolean isZero() {
		for (int x = 0; x < getRows(); x++)
			for (int y = 0; y < getCols(); y++)
				if (!vals[x][y].isZero())
					return false;
		return true;
	}

	public final ITensor itinverse() {
		return minverse();
	}

	public final ITensor matrixDvalue(final char letter, final double value) {
		final Expression[][] toret = new Expression[vals.length][vals[0].length];
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				toret[x][y] = vals[x][y].idouvalue(letter, value);
		return new Matrix(toret);
	}

	public final ITensor matrixDvalue(final IValueList valueList) {
		final Expression[][] toret = new Expression[vals.length][vals[0].length];
		for (int x = 0; x < vals.length; x++)
			for (int y = 0; y < vals[0].length; y++)
				toret[x][y] = vals[x][y].idouvalue(valueList);
		return new Matrix(toret);
	}

	public final Matrix minverse() {
		return MatrixMath.inverse(this);
	}

	public final ITensor multiply(final ITensor expr) {
		if (expr instanceof Matrix)
			return multiply((Matrix) expr);
		return doMultiply(expr);
	}

	@NotNull
	public final Matrix multiply(final Matrix other) {
		if (getCols() != other.getRows()) {
			Log.log(LogLevel.ERROR, "Wrong dimensions in Matrix multiply");
			return Matrix.ZERO;
		}
		final Matrix toret = new Matrix(getRows(), other.getCols());
		for (int y = 0; y < getRows(); y++)
			for (int x = 0; x < other.getCols(); x++) {
				Expression toSet = Int.ZERO;
				for (int j = 0; j < getCols(); j++) {
					final ICalculable elem = get(y, j);
					final Expression elemOther = other.get(j, x);
					final Expression elemProduct = elem.multiply(elemOther);
					if (j == 0)
						toSet = elemProduct;
					else
						toSet = toSet.add(elemProduct);
				}
				toret.vals[y][x] = toSet;
			}
		return toret;
	}

	public final boolean needParenthesis() {
		return false;
	}

	public final Expression partialSubstitute(final char letter, final double value) {
		final Expression[][] toret = new Expression[getRows()][getCols()];
		for (int x = 0; x < getCols(); x++)
			for (int y = 0; y < getRows(); y++)
				toret[y][x] = vals[y][x].partialSubstitute(letter, value);
		return new Matrix(toret);
	}

	public final Expression partialSubstitute(final char letter, final Expression value) {
		final Expression[][] toret = new Expression[getRows()][getCols()];
		for (int x = 0; x < getCols(); x++)
			for (int y = 0; y < getRows(); y++)
				toret[y][x] = vals[y][x].partialSubstitute(letter, value);
		return new Matrix(toret);
	}

	public final Expression partialSubstitute(final IValueList valueList) {
		final Expression[][] toret = new Expression[getRows()][getCols()];
		for (int x = 0; x < getCols(); x++)
			for (int y = 0; y < getRows(); y++)
				toret[y][x] = vals[y][x].partialSubstitute(valueList);
		return new Matrix(toret);
	}

	private void set(final Expression[] values) {
		for (int y = 0, i = 0; y < getRows(); y++)
			for (int x = 0; x < getCols(); x++, i++)
				vals[y][x] = values[i];
	}

	private void set(final Expression[][] values) {
		for (int y = 0; y < getRows(); y++)
			if (getCols() >= 0) System.arraycopy(values[y], 0, vals[y], 0, getCols());
	}

	public final void toFullString(final StringBuffer buffer) {
		buffer.append("Mat{");
		for (int y = 0; y < getRows(); y++) {
			if (y > 0)
				buffer.append(",{");
			else if (getRows() > 1)
				buffer.append('{');
			for (int x = 0; x < getCols() - 1; x++) {
				vals[y][x].toFullString(buffer);
				buffer.append(',');
			}
			if (getCols() > 0)
				vals[y][getCols() - 1].toFullString(buffer);
			if (getRows() > 1)
				buffer.append('}');
		}
		buffer.append('}');
	}

	public final void toString(final StringBuffer toret) {
		toret.append('{');
		for (int y = 0; y < getRows(); y++) {
			if (y > 0)
				toret.append(",{");
			else if (getRows() > 1)
				toret.append('{');
			for (int x = 0; x < getCols() - 1; x++) {
				if (vals[y][x] == null)
					toret.append("null");
				else
					vals[y][x].toString(toret);
				toret.append(',');
			}
			if (getCols() >= 1)
				if (vals[y][getCols() - 1] == null)
					toret.append("null");
				else
					vals[y][getCols() - 1].toString(toret);
			if (getRows() > 1)
				toret.append('}');
		}
		toret.append('}');
	}

	public final INumber value(final IValueList letters) {
		if (getRows() * getCols() == 2) {
			if (getRows() == 1)
				return new Complex(vals[0][0].value(letters), vals[0][1].value(letters));
			return new Complex(vals[0][0].value(letters), vals[1][0].value(letters));
		}
		return vals[0][0].value(letters);
	}

	public Expression accept(final IExpressionTensorVisitor visitor) {
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer) {
		buffer.append("(\\table ");
		for (int y = 0; y < getRows(); y++) {
			if (y > 0)
				buffer.append("; ");
			for (int x = 0; x < getCols(); x++) {
				if (vals[y][x] == null)
					buffer.append("null");
				else
					vals[y][x].toWrappedHtml(buffer);
				if (x != getCols() - 1)
					buffer.append(',');
			}
		}
		buffer.append(')');
	}
}
