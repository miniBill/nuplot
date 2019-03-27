package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import meplot.algebra.CyclicElement;

import org.junit.Test;

public class AlgebraTest{
	@Test
	public void testCyclic(){
		final CyclicElement arg = new CyclicElement(3, 5);
		assertEquals("Incorrect value", "CE[3,5]", arg.toFullString());
		final CyclicElement sum = arg.add(arg);
		assertEquals("Incorrect value", "CE[1,5]", sum.toFullString());
		final CyclicElement product = arg.multiply(arg);
		assertEquals("Incorrect value", "CE[4,5]", product.toFullString());
		final CyclicElement inverse = arg.cinverse();
		assertEquals("Incorrect value", "CE[2,5]", inverse.toFullString());
	}

	@Test
	public void testFullCyclic(){
		final int[] primes = new int[]{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
				37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97};
		for(final int prime : primes)
			for(int j = 1; j < prime; j++){
				final CyclicElement curr = new CyclicElement(j, prime);
				final CyclicElement opp = curr.copposite();
				final CyclicElement zero = curr.add(opp);
				assertTrue(
						"Nonzero: " + curr.toFullString() + "+"
								+ opp.toFullString(), zero.isZero());
				final CyclicElement inv = curr.cinverse();
				final CyclicElement one = curr.multiply(inv);
				assertTrue(
						"Nonone: " + curr.toFullString() + "*"
								+ inv.toFullString(), one.isOne());
			}
	}
}
