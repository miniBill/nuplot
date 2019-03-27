package meplot.numerical;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.geometry.ITensor;
import meplot.expressions.geometry.Tensor;
import meplot.expressions.list.IValueList;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public final class RungeKutta{
	private final double startt;
	private final Expression startx;
	private final IValue deltax;
	private final ISubstitutible lambda;

	public RungeKutta(final IValue deltax, final double startt, final Expression startx, final ISubstitutible lambda){
		this.deltax = deltax;
		this.startt = startt;
		this.startx = startx;
		this.lambda = lambda;
	}

	private double step;
	private double currt;
	private IValueList status;
	private Dou halfstep;
	private Dou fullstep;
	private boolean first;

	public void start(final double step){
		this.step = step;
		currt = startt;
		final Expression x0s = SimplificationHelper.simplify(startx);
		if(x0s instanceof Tensor)
			status = new ValueList('t', new Dou(currt), 'x', x0s);
		else
			status = new ValueList('t', new Dou(currt), 'x', Int.ZERO);
		halfstep = new Dou(step / 2.0);
		fullstep = new Dou(step / 2.0);
		first = true;
	}

	public Expression next(){
		if(status == null)
			start(0);
		final Tensor currx = (Tensor)status.value('x');
		if(first){
			first = false;
			return lambda.partialSubstitute('t', currt).multiply(currx);
		}
		final Dou midstep = new Dou(currt + step / 2.0);
		final Dou nextstep = new Dou(currt + step);
		final ITensor rk1 = deltax.matrixDvalue(status);
		status.set('t', midstep);
		status.set('x', currx.add(halfstep.multiply(rk1)));
		final ITensor rk2 = deltax.matrixDvalue(status);
		status.set('x', currx.add(halfstep.multiply(rk2)));
		final ITensor rk3 = deltax.matrixDvalue(status);
		status.set('t', nextstep);
		status.set('x', currx.add(fullstep.multiply(rk3)));
		final ITensor rk4 = deltax.matrixDvalue(status);
		final ITensor brk2 = rk2.multiply(Int.TWO);
		final ITensor brk3 = rk3.multiply(Int.TWO);
		final ITensor sum = rk1.add(brk2).add(brk3).add(rk4);
		final ITensor halfsum = halfstep.multiply(sum);
		final ITensor divided = halfsum.divide(Int.THREE);
		final ITensor nextx = currx.add(divided);
		status.set('x', nextx);
		final Expression toret = lambda.partialSubstitute('t', currt).multiply(nextx);
		currt += step;
		return toret;
	}

	public double getT(){
		return currt;
	}

	public double getStep(){
		return step;
	}
}
