package test;

import static org.junit.Assert.assertEquals;
import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.Int;
import meplot.expressions.other.Poly;
import meplot.expressions.other.PolynomialMath;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.Parser;
import meplot.solver.AbstractSolver;

import org.junit.Test;
import platform.lists.IList;

public class RootFinderTest extends TestUtils{
	@Test
	public void testFold(){
		final IExpressionList test = new ExpressionList();
		test.add(Int.ONE);
		test.add(Int.ZERO);
		test.add(Letter.I);
		test.add(Int.ONE);
		test.add(Parser.parseOrDefault("0"));
		test.add(new Letter('i'));// EXPLICITLY creating a new copy
		final IExpressionList folded = test.fold();
		assertEquals("1,0,i", folded.toString());
	}

	@Test
	public void testDivisors(){
		final IList<Expression> div = PolynomialMath.divisors(new Int(36));
		assertEquals("1,2,3,4,6,9,12,18,36", ExpressionList.toString(div));
	}

	@Test
	public void testFractionDivisors(){
		final IList<Expression> fdiv = PolynomialMath.divisors(new Fraction(new Int(10),
				new Int(14)));
		assertEquals("1,2,5,10,1/2,5/2,1/7,2/7,5/7,10/7,1/14,5/14", ExpressionList.toString(fdiv));
	}

	@Test
	public void testSimplification(){
		final Expression a = parseOrFail("x^3-1");
		final Expression b = parseOrFail("x-1");
		final Poly pa = new Poly(a, 'x');
		final Poly pb = new Poly(b, 'x');
		final Object m = pa.mod(pb);
		m.toString();
		final Expression g = FunctionsMath.gcd(a, b);
		AbstractSolver.activateCross();
		final Object d = SimplificationHelper.simplify(a.divide(g));
		AbstractSolver.deactivateCross();
		assertEquals("x^2+x+1", d.toString());
	}
}
