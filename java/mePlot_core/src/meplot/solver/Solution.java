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
		return "Passaggi:\n" + steps.stringFold().toCleanString('\n');
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append("Passaggi:<br/>\n");
		steps.toHtml(buffer);
	}
}
