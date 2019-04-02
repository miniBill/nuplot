package meplot.expressions.tree;

import java.util.Stack;

class ExpressionTreeIteratorImpl extends ExpressionTreeIterator {
	private final Stack<ExpressionTree> stack = new Stack<>();

	ExpressionTreeIteratorImpl(final ExpressionTree head) {
		stack.push(head);
	}

	public ExpressionTree next() {
		if (hasNext()) {
			final ExpressionTree curr = pop();
			if (curr.hasBrother())
				stack.push(curr.getBrother());
			if (curr.hasChild())
				stack.push(curr.getChild());
			return curr;
		}
		return null;
	}

	private ExpressionTree pop() {
		return stack.pop();
	}

	public boolean hasNext() {
		return !stack.empty();
	}

	public ExpressionTree peek() {
		return stack.peek();
	}

	public ExpressionTreeIterator subIterator() {
		return new ExpressionTreeIteratorImpl(stack.peek());
	}
}
