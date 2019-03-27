package meplot.expressions.visitors.derivative;

import meplot.expressions.Expression;
import meplot.expressions.functions.MonicFunction;
import meplot.expressions.functions.complex.ComplexFunction;
import meplot.expressions.functions.exp.Cbrt;
import meplot.expressions.functions.exp.Exp;
import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.functions.exp.Ln;
import meplot.expressions.functions.exp.Log10;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.operations.Derivative;
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
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public class DerivativeMonicFunctionVisitor implements IExpressionMonicFunctionVisitor{
	private final DerivativeVisitor parent;

	public DerivativeMonicFunctionVisitor(final DerivativeVisitor parent){
		this.parent = parent;
	}

	private Expression composite(final Expression base, final MonicFunction function){
		return base.multiply(parent.genvisit(function.getArgument()));
	}

	public Expression visit(final Abs abs){
		return composite(new Sign(abs.getArgument()), abs);
	}

	public Expression visit(final Asin asin){
		final Expression step = asin.getArgument().square().opposite();
		return composite(new Sqrt(Int.ONE.add(step)).inverse(), asin);
	}

	public Expression visit(final Atan atan){
		final Expression base = Int.ONE.add(atan.getArgument().square()).inverse();
		return composite(base, atan);
	}

	public Expression visit(final Cbrt cbrt){
		return composite(Int.THREE.divide(cbrt.square()), cbrt);
	}

	public Expression visit(final ComplexFunction complexFunction){
		return Int.ZERO;
	}

	public Expression visit(final Cos cos){
		return composite(new Sin(cos.getArgument()).opposite(), cos);
	}

	public Expression visit(final Cosh cosh){
		return composite(new Sinh(cosh.getArgument()), cosh);
	}

	public Expression visit(final Exp exp){
		return composite(exp, exp);
	}

	public Expression visit(final Floor floor){
		return Int.ZERO;
	}

	public Expression visit(final Hold hold){
		return new Hold(new Derivative(hold.getArgument(), parent.getVariable()));
	}

	public Expression visit(final Ln ln){
		return composite(ln.getArgument().inverse(), ln);
	}

	public Expression visit(final Log10 log10){
		return composite(log10.getArgument().multiply(ExpMath.LNTEN).inverse(), log10);
	}

	public Expression visit(final Sign sign){
		return Int.ZERO;
	}

	public Expression visit(final Sin sin){
		return composite(new Cos(sin.getArgument()), sin);
	}

	public Expression visit(final Sinh sinh){
		return composite(new Cosh(sinh.getArgument()), sinh);
	}

	public Expression visit(final Sqrt sqrt){
		return composite(Int.TWO.multiply(sqrt).inverse(), sqrt);
	}

	public Expression visit(final Tan tan){
		return composite(Int.ONE.add(tan.square()), tan);
	}
}
