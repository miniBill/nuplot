package meplot.expressions.tree;


import platform.lists.ToStringList;

final class ExpressionTreeLeafIterator extends ExpressionTreeIterator{
	private final ExpressionTreeIterator iterator;

	ExpressionTreeLeafIterator(final ExpressionTreeIterator head){
		iterator = head;
	}

	public ExpressionTree next(){
		// Remember that hasNext() does iterator.next()
		// until it finds a suitable node
		if(hasNext())
			return iterator.next();
		return null;
	}

	public ExpressionTree peek(){
		// Remember that hasNext() does iterator.next()
		// until it finds a suitable node
		if(hasNext())
			return iterator.peek();
		return null;
	}

	public boolean hasNext(){
		while(iterator.hasNext()){
			final ExpressionTree curr = iterator.peek();
			if(!curr.hasChild())
				return true;
			iterator.next();
		}
		return false;
	}

	public String toString(){
		final ExpressionTreeIterator clone = subIterator();
		final StringBuilder buffer = new StringBuilder();
		while(clone.hasNext()){
			ToStringList.toString(clone.next().getValue(), buffer);
			if(clone.hasNext())
				buffer.append(',');
		}
		return buffer.toString();
	}

	public ExpressionTreeIterator subIterator(){
		return new ExpressionTreeLeafIterator(iterator.subIterator());
	}
}
