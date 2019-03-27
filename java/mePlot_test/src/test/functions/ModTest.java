package test.functions;

import org.junit.Test;

import test.TestUtils;

public final class ModTest extends TestUtils{
	@Test
	public void testMod(){
		assertSimplify("5%3", "2");
		assertSimplify("(-1)%3", "-1");
		assertSimplify("e%x", "e%x");
	}
}
