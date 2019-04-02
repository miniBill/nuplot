package meplot.expressions.geometry;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.exceptions.InversionException;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import platform.NotImplementedException;

public final class MatrixMath{
	private MatrixMath(){
	}

	public static Matrix inverse(final Matrix mat){
		if(mat.getRows() != mat.getCols())
			throw new InversionException("Trying to invert nonsquare matrix");

		final Expression[][] temp = new Expression[mat.getRows()][mat.getCols() * 2];

		// set initial matrix
		for(int y = 0; y < mat.getRows(); y++)
			for(int x = 0; x < mat.getCols(); x++){
				temp[y][x] = mat.get(y, x);
				temp[y][x + mat.getCols()] = x == y ? Int.ONE : Int.ZERO;
			}

		for(int col = 0; col < mat.getCols(); col++){
			// search for a leading nonzero
			if(!findLeadingNonzeroForInv(temp, col))
				throw new InversionException("Trying to invert singular matrix");
			final Expression lead = SimplificationHelper.simplify(temp[col][col].inverse());
			if(!lead.isOne())
				opII(temp, col, lead);
			for(int y = 0; y < mat.getRows(); y++)
				if(y != col && !temp[y][col].isZero())
					opIII(temp, col, y, temp[y][col].opposite());
		}
		return rightMatrix(temp, mat.getCols());
	}

	private static Matrix rightMatrix(final Expression[][] vals, final int columns){
		final Expression[][] toret = new Expression[vals.length][columns];
		for(int y = 0; y < vals.length; y++)
			if (columns >= 0) System.arraycopy(vals[y], vals[0].length - columns, toret[y], 0, columns);
		return new Matrix(toret);
	}

	private static boolean findLeadingNonzeroForInv(final ICalculable[][] mat,
			final int col){
		for(int y = col; y < mat.length; y++)
			if(!mat[y][col].isZero()){
				if(y != col)
					opI(mat, y, col);
				return true;
			}
		return false;
	}

	private static void opIII(final ICalculable[][] mat, final int rowFrom,
			final int rowTo, final Expression coeff){
		for(int col = 0; col < mat[0].length; col++){
			final ICalculable exprFrom = mat[rowFrom][col];
			final Expression exprToAdd = exprFrom.multiply(coeff);
			final ICalculable exprTo = mat[rowTo][col];
			final Expression exprFinal = exprTo.add(exprToAdd);
			mat[rowTo][col] = SimplificationHelper.simplify(exprFinal);
		}
	}

	private static void opII(final ICalculable[][] mat, final int row,
			final Expression coeff){
		for(int col = 0; col < mat[0].length; col++)
			mat[row][col] = SimplificationHelper.simplify(mat[row][col].multiply(coeff));
	}

	private static void opI(final ICalculable[][] mat, final int rowFrom, final int rowTo){
		for(int col = 0; col < mat[0].length; col++){
			final ICalculable temp = mat[rowFrom][col];
			mat[rowFrom][col] = mat[rowTo][col];
			mat[rowTo][col] = temp;
		}
	}

	public static Expression det(final Matrix mat){
		if(mat.getRows() != mat.getCols())
			throw new DeterminantException("Asked for determinant of nonsquare matrix");

		Expression toret = Int.ONE;
		final Expression[][] temp = mat.deepCopy();

		for(int col = 0; col < mat.getCols(); col++){
			// search for a leading nonzero
			toret = findLeadingNonzeroForDet(temp, col, toret);
			if(toret.isZero())
				return Int.ZERO;

			final Expression lead = SimplificationHelper.simplify(temp[col][col].inverse());
			if(!lead.isOne()){
				opII(temp, col, lead);
				toret = toret.divide(lead);
			}
			for(int y = col + 1; y < mat.getRows(); y++)
				if(!temp[y][col].isZero())
					opIII(temp, col, y, temp[y][col].opposite());
		}
		for(int row = 0; row < mat.getRows(); row++)
			toret = toret.multiply(temp[row][row]);
		return toret;
	}

	private static Expression findLeadingNonzeroForDet(final ICalculable[][] mat,
			final int col, final Expression toret){
		for(int y = col; y < mat.length; y++)
			if(!mat[y][col].isZero()){
				if(y != col){
					opI(mat, y, col);
					return toret.opposite();
				}
				return toret;
			}
		return Int.ZERO;
	}

	public static Matrix transpose(final Matrix mat){
		final Expression[][] toret = new Expression[mat.getCols()][mat.getRows()];
		for(int x = 0; x < toret.length; x++)
			for(int y = 0; y < toret[0].length; y++)
				toret[x][y] = mat.get(y, x);
		return new Matrix(toret);
	}

	public static Int rank(final Matrix mat){
		final ICalculable[][] temp = mat.deepCopy();

		for(int col = 0; col < mat.getRows(); col++){
			if(col == mat.getCols())
				return new Int(col);
			// search for a leading nonzero
			if(!findLeadingNonzeroForInv(temp, col))
				return new Int(mat.getRows() - col);

			final Expression lead = SimplificationHelper.simplify(temp[col][col].inverse());
			if(!lead.isOne())
				opII(temp, col, lead);
			for(int y = col + 1; y < mat.getRows() && y < mat.getCols(); y++)
				if(!temp[y][col].isZero())
					opIII(temp, col, y, temp[y][col].opposite());
		}
		return new Int(mat.getCols());
	}

}
