package meplot.expressions;

public interface ICalculable{
	Expression add(Expression other);

	boolean compatible(Expression other, char operation);

	Expression divide(Expression other);

	Expression inverse();

	boolean isOne();

	boolean isZero();

	Expression multiply(Expression other);

	Expression opposite();

	Expression square();
}
