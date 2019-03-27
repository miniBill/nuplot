package test;

import org.junit.Test;

public class ComplexTest extends TestUtils{
	@Test
	public void testRe(){
		assertSimplify("re(x+iy)", "re(x)-im(y)");
		assertSimplify("im(x+iy)", "im(x)+re(y)");
	}
}
