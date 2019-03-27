package meplot.parser.tokens;

import meplot.expressions.functions.UserFunction;
import platform.lists.List;

public final class UserFunctionList extends List{
	public UserFunctionList(final UserFunction fun){
		add(fun);
	}

	/**
	 * Creates an empty list.
	 */
	public UserFunctionList(){
		// Creates an empty list.
	}

	public void add(final UserFunction fun){
		super.add(fun);
	}

	public void addRange(final UserFunctionList toadd){
		final UserFunctionIterator iterator = toadd.getIterator();
		while(iterator.hasNext())
			add(iterator.next());
	}

	public UserFunction elementAt(final int index){
		return (UserFunction)gelementAt(index);
	}

	public UserFunctionIterator getIterator(){
		return new UserFunctionIterator(this, 0);
	}
}
