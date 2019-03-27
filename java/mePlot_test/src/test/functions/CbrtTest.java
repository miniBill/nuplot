package test.functions;

import org.junit.Test;

import test.TestUtils;

public class CbrtTest extends TestUtils{
	@Test
	public void testCbrt(){
		assertSimplify("cbrt125", "5");
	}
}
