package form;

import meplot.expressions.Expression;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.Parser;
import meplot.parser.utils.Cleaner;

public final class MainClass{
	private MainClass(){
	}

	public static void main(final String[] args){
		final Expression multiVar = Parser.parseOrDefault("-xx-yy");
		check(multiVar, 'x');

		final Expression space = Parser.parseOrDefault("s+vt+1/2at^2");
		check(space, 't');

		final Expression unintegrable = Parser.parseOrDefault("e^(-xx)");
		check(unintegrable, 'x');
	}

	private static void check(final Expression input, final char var){
		final Expression space = SimplificationHelper.simplify(input);
		System.out.println("f(" + var + ")=" + Cleaner.clean(space.toString()));

		final Expression speed = SimplificationHelper.simplify(DerivativeHelper.derivativeOrDefault(space, var));
		System.out.println("f'(" + var + ")=" + Cleaner.clean(speed.toString()));

		final Object accelleration = SimplificationHelper.simplify(DerivativeHelper.derivativeOrDefault(speed, var));
		System.out.println("f''(" + var + ")=" + Cleaner.clean(accelleration.toString()));
	}
}
