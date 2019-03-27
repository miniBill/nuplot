package meplot.expressions.geometry;

import meplot.expressions.Expression;
import meplot.expressions.list.IExpressionIterator;
import platform.lists.IToString;

public class MatrixElementsIterator implements IExpressionIterator{
	private int row = 0;
	private int col = 0;
	private final Matrix matrix;

	public MatrixElementsIterator(Matrix matrix){
		this.matrix = matrix;
	}

	private MatrixElementsIterator(Matrix matrix, int row, int col){
		this.matrix = matrix;
		this.row = row;
		this.col = col;
	}

	public Expression next(){
		Expression toret = getCurrent();
		col++;
		if(col == matrix.getCols()){
			col = 0;
			row++;
		}
		return toret;
	}

	public IExpressionIterator subIterator(){
		return new MatrixElementsIterator(matrix, row, col);
	}

	public Expression getCurrent(){
		if(row == matrix.getRows())
			return null;
		return matrix.get(row, col);
	}

	public Expression[] toArray(){
		final Expression[] toret = new Expression[length() - row * matrix.getCols() - col];
		int i = 0;
		for(int r = row; r < matrix.getRows(); r++)
			for(int c = (r == row ? col : 0); c < matrix.getCols(); c++)
				toret[i++] = matrix.get(r, c);
		return toret;
	}

	public Expression getLast(){
		return matrix.get(matrix.getRows() - 1, matrix.getCols() - 1);
	}

	public IToString tnext(){
		return next();
	}

	public boolean hasNext(){
		return row != matrix.getRows();
	}

	public int length(){
		return matrix.getRows() * matrix.getCols();
	}

	public boolean contains(Object arg){
		for(int r = row; r < matrix.getRows(); r++)
			for(int c = (r == row ? col : 0); c < matrix.getCols(); c++)
				if(matrix.get(r, c).equals(arg))
					return true;
		return false;
	}

	public Object enext(){
		return next();
	}

	public boolean isEmpty(){
		return length() == 0;
	}

	public boolean isSingle(){
		return length() == 1;
	}
}
