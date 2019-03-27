package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.functions.operations.Mod;
import meplot.expressions.functions.other.Ackermann;
import meplot.expressions.functions.other.Gcd;
import meplot.expressions.functions.other.Mandelbrot;
import meplot.expressions.functions.piecewise.Max;
import meplot.expressions.functions.piecewise.Min;
import meplot.expressions.functions.piecewise.Piecewise;

public interface IExpressionNonsymbolicFunctionVisitor{
	Expression visit(Ackermann ackermann);

	Expression visit(Gcd gcd);

	Expression visit(Mandelbrot mandelbrot);

	Expression visit(Max max);

	Expression visit(Min min);

	Expression visit(Mod mod);

	Expression visit(Piecewise piecewise);
}
