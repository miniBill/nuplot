package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.functions.Function;
import meplot.expressions.functions.exp.Ln;
import meplot.expressions.functions.exp.Sqrt;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.functions.piecewise.IPower;
import meplot.expressions.functions.trig.Cos;
import meplot.expressions.functions.trig.Sin;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.list.IValueList;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionVisitor;

public final class Power extends AbstractExpression implements IPower{
	private final Expression base;
	private final Expression exponent;

	public Power(final Expression base, final Expression exponent){
		this.base = base;
		this.exponent = exponent;
	}

	public void toString(final StringBuffer toret){
		if(base instanceof Letter || base instanceof Function || base instanceof IInt && ((IInt)base).getValue() >= 0)
			toret.append(base);
		else{
			toret.append('(');
			toret.append(base);
			toret.append(')');
		}
		toret.append('^');
		if(exponent instanceof Letter || exponent instanceof IInt || exponent instanceof Matrix)
			toret.append(exponent);
		else{
			toret.append('(');
			toret.append(exponent);
			toret.append(')');
		}
	}

	public boolean isOne(){
		return base.isOne() || exponent.isZero();
	}

	public boolean isZero(){
		return base.isZero();
	}

	public Expression multiply(final Expression other){
		if(base.equals(other))
			return new Power(base, exponent.add(Int.ONE));
		if(other instanceof Power){
			Power pother = (Power)other;
			if(pother.base.equals(base))
				return new Power(base, exponent.add(pother.exponent));
		}

		return super.multiply(other);
	}

	public Expression divide(final Expression other){
		if(base.equals(other))
			return new Power(base, exponent.add(Int.MINUSONE));
		if(other instanceof Power){
			Power pother = (Power)other;
			if(pother.base.equals(base))
				return new Power(base, exponent.add(pother.exponent.opposite()));
		}
		return super.divide(other);
	}

	public Expression add(final Expression other){
		if(other instanceof Power && exponent.equals(Int.TWO)){
			final Power oth = (Power)other;
			if(oth.exponent.equals(Int.TWO)){
				if(base instanceof Sin && oth.base instanceof Cos
						&& ((Sin)base).getArgument().equals(((Cos)oth.base).getArgument()))
					return Int.ONE;

				if(base instanceof Cos && oth.base instanceof Sin
						&& ((Cos)base).getArgument().equals(((Sin)oth.base).getArgument()))
					return Int.ONE;
			}
		}
		return super.add(other);
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if((operation == Operation.MULTIPLICATION || operation == Operation.DIVISION) && elementAt.equals(base))
			return true;
		if(elementAt instanceof Power){
			if((operation == Operation.MULTIPLICATION || operation == Operation.DIVISION)
					&& ((Power)elementAt).base.equals(base))
				return true;
			if(operation == Operation.ADDITION && exponent.equals(Int.TWO)){
				final Power oth = (Power)elementAt;
				if(oth.exponent.equals(Int.TWO)){
					if(base instanceof Sin && oth.base instanceof Cos
							&& ((Sin)base).getArgument().equals(((Cos)oth.base).getArgument()))
						return true;

					if(base instanceof Cos && oth.base instanceof Sin
							&& ((Cos)base).getArgument().equals(((Sin)oth.base).getArgument()))
						return true;
				}
			}
		}
		return super.compatible(elementAt, operation);
	}

	public Expression innerSimplify(){
		if(isZero())
			return Int.ZERO;
		if(isOne())
			return Int.ONE;
		final Expression sexp = exponent.partialSimplify();
		final Expression sbase = base.partialSimplify();
		return finishSimplification(sbase, sexp);
	}

	private Expression finishSimplification(final Expression sbase, final Expression sexp){
		if(sexp.isOne())
			return sbase;
		if(sbase instanceof Power)
			return finishSimplificationPower((Power)sbase, sexp);
		if(sbase instanceof Abs && sexp instanceof IInt && ((IInt)sexp).getValue() % 2 == 0)
			return new Power(((Abs)sbase).getArgument(), sexp);
		if(sbase instanceof IReal && sexp instanceof IInt)
			return finishSimplificationRealInt((IReal)sbase, (IInt)sexp);
		if(sexp instanceof IReal && ((IReal)sexp).isNegative())
			return new Power(sbase, sexp.opposite()).inverse();
		if(sbase instanceof Letter)
			return finishSimplificationLetter((Letter)sbase, sexp);
		if(sbase instanceof Complex)
			return finishSimplificationComplex((Complex)sbase, sexp);
		if(sexp instanceof Fraction && sexp.equals(Fraction.ONEHALF))
			return new Sqrt(sbase);
		if(sbase instanceof Multiplication)
			return finishSimplificationMultiplication((Multiplication)sbase, sexp);
		if(sexp.equals(Int.MINUSONE))
			return sbase.inverse();
		if(sexp instanceof IReal && ((IReal)sexp).isNegative())
			return new Power(sbase, sexp.opposite()).inverse();
		if(sbase instanceof Sqrt && sexp instanceof IInt)
			return finishSimplificationSqrtInt((Sqrt)sbase, (IInt)sexp);
		if(sbase instanceof IDivision){
			final IDivision dsbase = (IDivision)sbase;
			final Power num = new Power(dsbase.getNumerator(), sexp);
			final Power den = new Power(dsbase.getDenominator(), sexp);
			return new Division(num, den);
		}
		if(isSortOfNumber(sbase) && sexp instanceof IInt)
			return expand(sbase, (IInt)sexp);
		return new Power(sbase, sexp);
	}

	private static boolean isSortOfNumber(final Expression expr){
		if(expr instanceof INumber)
			return true;
		IExpressionIterator inner = null;
		if(expr instanceof Sum){
			final Sum sexpr = (Sum)expr;
			inner = sexpr.getAddends();
		}
		if(expr instanceof Multiplication){
			final Multiplication mexpr = (Multiplication)expr;
			inner = mexpr.getFactors();
		}
		if(expr instanceof Sqrt)
			return isSortOfNumber(((Sqrt)expr).getArgument());
		if(inner == null)
			return false;
		while(inner.hasNext())
			if(!isSortOfNumber(inner.next()))
				return false;
		return true;
	}

	private static Expression finishSimplificationPower(final Power pbase, final Expression sexp){
		return new Power(pbase.base, pbase.exponent.multiply(sexp));
	}

	private static Expression finishSimplificationSqrtInt(final Sqrt ssbase, final IInt sexp){
		if(sexp.getValue() == 2)
			return new Abs(ssbase.getArgument());
		if(sexp.getValue() % 2 == 0)
			return new Power(ssbase.getArgument(), new Int(sexp.getValue() / 2));
		return new Power(ssbase, sexp);
	}

	private static Expression finishSimplificationRealInt(final IReal rbase, final IInt iexp){
		if(rbase.isNegative()){
			if(iexp.getValue() % 2 == 0)
				return new Power(rbase.iropposite(), iexp);
			return new Power(rbase.iropposite(), iexp).opposite();
		}
		if(iexp.getValue() <= 6){
			final int maxBase = maxBase(iexp.getValue());
			if(rbase instanceof IInt){
				final int intbase = ((IInt)rbase).getValue();
				if(Math.abs(intbase) <= maxBase)
					return OperationsMath.pow(intbase, iexp);
			}
			else
				if(rbase instanceof Fraction){
					final Fraction frac = (Fraction)rbase;
					final int nbr = frac.fgetNumerator().getValue();
					final int dbr = frac.fgetDenominator().getValue();
					if(Math.abs(nbr) <= maxBase && Math.abs(dbr) <= maxBase){
						final IInt nexp = OperationsMath.pow(nbr, iexp);
						final IInt dexp = OperationsMath.pow(dbr, iexp);
						return new Fraction(nexp, dexp);
					}
				}
				else{
					final double doublebase = rbase.toDouble();
					if(Math.abs(doublebase) <= maxBase)
						return OperationsMath.pow(rbase, iexp);
				}
		}
		return new Power(rbase, iexp);
	}

	// We want to keep the result < 1 000 000
	private static int maxBase(final int value){
		switch(value){
			case 0:
				return Integer.MAX_VALUE;
			case 1:
				return Integer.MAX_VALUE;
			case 2:
				return 1000;
			case 3:
				return 100;
			case 4:
				return 50;
			case 5:
				return 25;
			case 6:
				return 12;
			default:
				return 1;
		}
	}

	private Expression finishSimplificationComplex(final Complex sbase, final Expression sexp){
		if(sexp instanceof IInt){
			if(Math.abs(((IInt)sexp).getValue()) < 4)
				return OperationsMath.pow(sbase, (IInt)sexp);
			if(sbase.equals(Complex.I)){
				final int iexp = ((IInt)sexp).getValue();
				switch(iexp % 4){
					case 0:
						return Int.ONE;
					case 1:
						return base;
					case 2:
						return Int.MINUSONE;
					default:
						return base.opposite();
				}
			}
		}

		return new Power(sbase, sexp);
	}

	public Expression innerStepSimplify(){
		if(isZero())
			return Int.ZERO;
		if(isOne())
			return Int.ONE;
		final Expression sexp = exponent.innerStepSimplify();
		final Expression sbase = base.innerStepSimplify();
		return finishSimplification(sbase, sexp);
	}

	private static Expression finishSimplificationLetter(final Letter base, final Expression exp){
		if(base.getLetter() == 'e'){
			if(exp instanceof Ln)
				return ((Ln)exp).getArgument();
			if(exp instanceof Letter && exp.equals(Letter.I))
				return new Cos(Int.ONE).add(Letter.I.multiply(new Sin(Int.ONE)));
			if(exp instanceof Multiplication){
				final Multiplication mulexp = (Multiplication)exp;
				if(mulexp.getFactors().contains(Letter.I)){
					final Expression arg = mulexp.divide(Letter.I);
					return new Cos(arg).add(Letter.I.multiply(new Sin(arg)));
				}
			}
		}
		return new Power(base, exp);
	}

	private static Expression finishSimplificationMultiplication(final Multiplication base, final Expression exp){
		final IExpressionList toret = new ExpressionList();
		final IExpressionIterator iterator = base.getFactors();
		while(iterator.hasNext())
			toret.add(new Power(iterator.next(), exp));
		return new Multiplication(toret);
	}

	public INumber value(final IValueList list){
		final INumber bval = base.value(list);
		final INumber eval = exponent.value(list);
		return OperationsMath.pow(bval, eval);
	}

	public double dvalue(final char letter, final double value){
		final INumber bval = base.value(letter, value);
		final INumber eval = exponent.value(letter, value);
		return OperationsMath.dpow(bval, eval);
	}

	public Expression partialSubstitute(final IValueList list){
		final Expression bpar = base.partialSubstitute(list);
		final Expression epar = exponent.partialSubstitute(list);
		return new Power(bpar, epar);
	}

	public Expression partialSubstitute(final char letter, final double value){
		final Expression bpar = base.partialSubstitute(letter, value);
		final Expression epar = exponent.partialSubstitute(letter, value);
		return new Power(bpar, epar);
	}

	public Expression partialSubstitute(final char letter, final Expression value){
		final Expression bpar = base.partialSubstitute(letter, value);
		final Expression epar = exponent.partialSubstitute(letter, value);
		return new Power(bpar, epar);
	}

	public Expression inverse(){
		if(exponent.isOne())
			return base.inverse();
		return super.inverse();
	}

	public Expression getExponent(){
		return exponent;
	}

	public Expression getBase(){
		return base;
	}

	public boolean hasLetter(final char var){
		return base.hasLetter(var) || exponent.hasLetter(var);
	}

	public boolean containsMatrix(){
		return base.containsMatrix() || exponent.containsMatrix();
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("^(");
		base.toFullString(buffer);
		buffer.append(',');
		exponent.toFullString(buffer);
		buffer.append(')');
	}

	public Expression expand(){
		if(exponent instanceof IInt)
			return expand(base, (IInt)exponent);
		return this;
	}

	private static Expression expand(final Expression base, final IInt iexp){
		if(iexp.getValue() == 0)
			return Int.ONE;
		if(iexp.getValue() == 1)
			return base;
		if(base instanceof IReal)
			return finishSimplificationRealInt((IReal)base, iexp);
		if(iexp.getValue() == 2 && base instanceof Sum){
			final IExpressionList toret = new ExpressionList();

			final Sum sbase = (Sum)base;

			final IExpressionIterator ita = sbase.getAddends();

			if(ita.length() == 2){
				final Expression a = ita.next();
				final Expression b = ita.next();
				toret.add(a.square());
				toret.add(Int.TWO.multiply(a.multiply(b)));
				toret.add(b.square());
			}
			else
				while(ita.hasNext()){
					final ICalculable curr = ita.next();
					final IExpressionIterator itb = sbase.getAddends();
					while(itb.hasNext())
						toret.add(curr.multiply(itb.next()));
				}

			return new Sum(toret);
		}
		return new Power(base, iexp);
	}

	public double fdvalue(final char letter, final double value){
		return OperationsMath.pow(base.fdvalue(letter, value), exponent.fdvalue(letter, value));
	}

	public boolean isFullDouble(){
		if(!(base instanceof IReal))
			return false;
		return ((IReal)base).isPositive() && exponent.isFullDouble();
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof Power))
			return false;
		final Power oth = (Power)other;
		return base.isIdentical(oth.base) && exponent.isIdentical(oth.exponent);
	}

	public Expression accept(final IExpressionVisitor visitor){
		return visitor.visit(this);
	}

	public void toHtml(final StringBuffer buffer){
		if(base instanceof Letter || base instanceof Function || base instanceof Int && ((Int)base).getValue() >= 0)
			base.toWrappedHtml(buffer);
		else{
			buffer.append('(');
			base.toHtml(buffer);
			buffer.append(')');
		}
		if(exponent instanceof Letter || exponent instanceof Int || exponent instanceof Matrix){
			buffer.append('^');
			exponent.toWrappedHtml(buffer);
		}
		else{
			buffer.append("^(");
			exponent.toHtml(buffer);
			buffer.append(")");
		}
	}
}
