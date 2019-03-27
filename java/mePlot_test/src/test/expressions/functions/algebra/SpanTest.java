package test.expressions.functions.algebra;

import org.junit.Test;

import test.TestUtils;

public class SpanTest extends TestUtils{
	@Test
	public void testOne(){
		assertSimplify("span1", "{1}");
	}

	@Test
	public void testZ2(){
		// assertSimplify("span(z=>(1-z),z=>(i-z))",
		// "{z=>z,z=>(1-z),z=>(i-z),z=>(i+1-z)}");
	}
}
