package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import platform.NotImplementedException;

public final class Indefinite extends Real implements IDou{
	public static final IDou ISTANCE = new Indefinite();

	private Indefinite(){

	}

	public IDou douvalue(final IValueList valueList){
		return this;
	}

	public boolean isIdentical(final Expression other){
		return other instanceof Indefinite;
	}

	public IReal add(final IReal arg){
		return this;
	}

	public IReal multiply(final IReal arg){
		return this;
	}

	public IReal divide(final IReal arg){
		return this;
	}

	public IReal iropposite(){
		return this;
	}

	public IReal irinverse(){
		return this;
	}

	public IReal abs(){
		return this;
	}

	public boolean isInt(){
		return false;
	}

	public double toDouble(){
		return Double.NaN;
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("INDEF");
	}

	public void toString(final StringBuffer buffer){
		buffer.append('¿');
	}

	public INumber insquare(){
		return this;
	}

	public void toHtml(final StringBuffer buffer){
		throw new NotImplementedException();
	}
}
