package meplot.expressions.geometry;

import meplot.expressions.Expression;

import java.util.Iterator;

public class MatrixElementsIterator implements Iterator<Expression> {
	private int row = 0;
	private int col = 0;
	private final Matrix matrix;

	public MatrixElementsIterator(Matrix matrix) {
		this.matrix = matrix;
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

	private Expression getCurrent() {
		if (row == matrix.getRows())
			return null;
		return matrix.get(row, col);
	}

	public boolean hasNext() {
		return row != matrix.getRows();
	}

}
