package test;

import static org.junit.Assert.assertEquals;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Int;

import org.junit.Test;

public final class ExpressionListTest extends TestUtils{
	@Test
	public void testAdd(){
		final IExpressionList list = new ExpressionList();
		list.add(Int.ZERO);
		assertEquals("0", list.toString('!'));
	}
}
