package test.functions;

import org.junit.Test;

import test.TestUtils;

public final class MinMaxTest extends TestUtils{
	@Test
	public void testMaxMin(){
		assertSimplify("max3,2", "3");
		assertSimplify("min3,2", "2");
	}
}
