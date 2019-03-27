package meplot.expressions.exceptions;

import meplot.localization.L10N;

public final class DerivationException extends CalcException{
	public DerivationException(){
		super(L10N.get(L10N.DERIVATIONEXCEPTION));
	}

	public DerivationException(final String message){
		super(message);
	}
}
