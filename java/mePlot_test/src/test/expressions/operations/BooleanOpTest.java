package test.expressions.operations;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;

import org.junit.Test;

import test.TestUtils;

public final class BooleanOpTest extends TestUtils{
	@Test
	public void testNumSimplify(){
		BooleanOp a = new BooleanOp(Int.MINUSONE, '<', Int.ONE);
		BooleanOp b = new BooleanOp(Int.ONE, '<', Int.MINUSONE);
		BooleanOp c = new BooleanOp(Int.MINUSONE, '>', Int.ONE);
		BooleanOp d = new BooleanOp(Int.ONE, '>', Int.MINUSONE);
		Expression[] toTest = new Expression[] {
				a, b, c, d
		};
		checkFullSimplify(toTest, "L(∀)", "L(∄)", "L(∄)", "L(∀)");
	}
}
