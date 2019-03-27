package meplot.expressions.visitors;

import meplot.expressions.Expression;
import meplot.expressions.functions.complex.ComplexFunction;
import meplot.expressions.functions.exp.Cbrt;
import meplot.expressions.functions.exp.Exp;
import meplot.expressions.functions.exp.Ln;
import meplot.expressions.functions.exp.Log10;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.other.Floor;
import meplot.expressions.functions.other.Hold;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.functions.piecewise.Sign;
import meplot.expressions.functions.trig.Asin;
import meplot.expressions.functions.trig.Atan;
import meplot.expressions.functions.trig.Cos;
import meplot.expressions.functions.trig.Cosh;
import meplot.expressions.functions.trig.Sin;
import meplot.expressions.functions.trig.Sinh;
import meplot.expressions.functions.trig.Tan;

public interface IExpressionMonicFunctionVisitor{
	Expression visit(Abs abs);

	Expression visit(Asin asin);

	Expression visit(Atan atan);

	Expression visit(Cbrt cbrt);

	Expression visit(ComplexFunction complexFunction);

	Expression visit(Cos cos);

	Expression visit(Cosh cosh);

	Expression visit(Exp exp);

	Expression visit(Floor floor);

	Expression visit(Hold hold);

	Expression visit(Ln ln);

	Expression visit(Log10 log10);

	Expression visit(Sign sign);

	Expression visit(Sin sin);

	Expression visit(Sinh sinh);

	Expression visit(Sqrt sqrt);

	Expression visit(Tan tan);
}
