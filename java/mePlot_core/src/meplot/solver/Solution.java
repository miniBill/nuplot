package meplot.solver;

import meplot.expressions.tree.ExpressionTree;

public final class Solution{
	private final ExpressionTree steps;

	public Solution(final ExpressionTree steps){
		this.steps = steps.stringFold();
	}

	public ExpressionTree getSteps(){
		return steps;
	}

	public String toString(){
		final StringBuffer buffer = new StringBuffer("Passaggi:\n");
		buffer.append(steps.stringFold().toCleanString('\n'));
		return buffer.toString();
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append("Passaggi:<br/>\n");
		steps.toHtml(buffer);
	}
}
