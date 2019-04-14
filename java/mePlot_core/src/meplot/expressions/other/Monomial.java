package meplot.expressions.other;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public final class Monomial extends Proxy{
	private final char var;
	private final int deg;
	private final Expression coeff;

	public Monomial(final Expression val, final char var){
		super(val);
		this.var = var;
		deg = Poly.getExponent(val, var);
		if(deg > 0)
			coeff = Poly.getCoefficent(val, var);
		else
			coeff = val;
	}

	private Monomial(final Expression coeff, final char var, final int deg){
		super(coeff.multiply(new Power(new Letter(var), new Int(deg))));
		this.coeff = coeff;
		this.var = var;
		this.deg = deg;
	}

	public Monomial add(final Monomial arg){
		if(isZero())
			return arg;
		if(arg.isZero())
			return this;
		if(arg.var != var)
			throw new CalcException("Trying to add monomials of different vars");
		return new Monomial(arg.coeff.add(coeff), var, deg);
	}

	public Monomial divide(final Monomial arg){
		if(arg.var != var)
			throw new CalcException("Trying to divide monomials of different vars");
		return new Monomial(coeff.divide(arg.coeff), var, deg - arg.deg);
	}

	public Expression innerSimplify(){
		if(deg == 0)
			return coeff;
		return super.innerSimplify();
	}

	public Monomial msimplify(){
		return new Monomial(SimplificationHelper.simplify(this), var);
	}

	public Monomial mopposite(){
		return new Monomial(super.opposite(), var);
	}

	public Expression getCoefficent(){
		return coeff;
	}

	public int getExponent(){
		return deg;
	}

	public void toString(final StringBuilder buffer){
		if(coeff.equals(Int.MINUSONE)){
			buffer.append('-');
			noCoeffString(buffer);
			return;
		}
		if(coeff.isOne()){
			noCoeffString(buffer);
			return;
		}

		coeff.toPString(buffer);

		if(deg == 0)
			return;

		buffer.append(var);
		if(deg != 1){
			buffer.append('^');
			buffer.append(deg);
		}
	}

	public void toFullString(final StringBuilder buffer){
		buffer.append("M(");
		coeff.toFullString(buffer);
		buffer.append(',');
		buffer.append(var);
		buffer.append(',');
		buffer.append(deg);
		buffer.append(')');
	}

	private void noCoeffString(final StringBuilder buffer){
		if(deg == 0){
			buffer.append('1');
			return;
		}
		buffer.append(var);
		if(deg != 1){
			buffer.append('^');
			buffer.append(deg);
		}
	}
}
