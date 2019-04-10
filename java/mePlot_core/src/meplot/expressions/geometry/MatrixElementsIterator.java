package meplot.expressions.geometry;

import meplot.expressions.Expression;
import platform.lists.IIterator;
import platform.lists.List;

public class MatrixElementsIterator implements IIterator<Expression> {
	private int row = 0;
	private int col = 0;
	private final Matrix matrix;

	public MatrixElementsIterator(Matrix matrix) {
		this.matrix = matrix;
	}

	private MatrixElementsIterator(Matrix matrix, int row, int col) {
		this.matrix = matrix;
		this.row = row;
		this.col = col;
	}

	public Expression next() {
		Expression toret = getCurrent();
		col++;
		if (col == matrix.getCols()) {
			col = 0;
			row++;
		}
		return toret;
	}

	@Override
	public IIterator<Expression> iterator() {
		return new MatrixElementsIterator(matrix, row, col);
	}

	public Expression getCurrent() {
		if (row == matrix.getRows())
			return null;
		return matrix.get(row, col);
	}

	public boolean hasNext() {
		return row != matrix.getRows();
	}

	public int length() {
		return matrix.getRows() * matrix.getCols();
	}

	public boolean contains(Expression arg) {
		for (int r = row; r < matrix.getRows(); r++)
			for (int c = (r == row ? col : 0); c < matrix.getCols(); c++)
				if (matrix.get(r, c).equals(arg))
					return true;
		return false;
	}

	@Override
	public boolean isSecond() {
		throw new RuntimeException("NIE");
	}
}
