package meplot.algebra;

import meplot.expressions.exceptions.CalcException;

final class CyclicException extends CalcException{
	CyclicException(final String message){
		super(message);
	}
}
