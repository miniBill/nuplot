package meplot.expressions;

public interface ISimplifiable{
	Expression innerSimplify();

	Expression innerStepSimplify();

	boolean isSimplified();

	Expression partialSimplify();
}
