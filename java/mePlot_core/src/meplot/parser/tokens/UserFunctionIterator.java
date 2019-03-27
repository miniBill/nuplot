package meplot.parser.tokens;

import meplot.expressions.functions.UserFunction;
import platform.lists.IIterable;
import platform.lists.Iterator;

final class UserFunctionIterator extends Iterator{
	private final UserFunctionList inner;

	UserFunctionIterator(final UserFunctionList head, final int index){
		super(index);
		inner = head;
	}

	public UserFunction next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
