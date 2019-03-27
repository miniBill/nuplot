package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Random;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.IOutputtable;
import meplot.expressions.ISubstitutible;
import meplot.expressions.IValue;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.InversionException;
import meplot.expressions.geometry.DeterminantException;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.geometry.MatrixMath;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Multiplication;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.utils.Cleaner;

import org.junit.Test;

import test.solver.SolverUtils;

public class MatrixTest extends TestUtils{
	@Test
	public void testSingle(){
		final Expression expr = parseOrFail("det{a}");
		assertEquals("Couldn't do singlet determinant.", "a", SimplificationHelper.simplify(expr).toString());
		final Matrix matA = matrixOrFail("{a}");
		final double value = matA.dvalue('a', 0.123);
		assertEquals("Couldn't extract matrix value.", 0.123, value, 0);
	}

	@Test
	public void testDerivative(){
		final Matrix a = matrixOrFail("{xx,xy,sinx}");
		final Matrix b = MatrixMath.transpose(a);
		final Object c = SimplificationHelper.simplify(DerivativeHelper.derivativeOrDefault(b, 'x'));
		assertEquals("Failed derivative", "{{2x},{y},{cos(x)}}", c.toString());
	}

	@Test
	public void testGradient(){
		final Matrix a = msimplify(matrixOrFail("{sinxcosy,yy}"));
		final Matrix da = msimplify(a.gradient());
		assertEquals("Failed gradient", "{cos(y)cos(x),2y}", da.toString());
	}

	private static Matrix msimplify(final Matrix gradient){
		final Expression toret = SimplificationHelper.simplify(gradient);
		if(toret instanceof Matrix)
			return (Matrix)toret;
		fail("Matrix didn't simplify to matrix");
		return null;
	}

	@Test
	public void testEigen(){
		// Solver.activateCross();
		// assertSimplify("det({{1,1},{0,1}}-x)", "(1-x)^2");
		// assertSimplify("det({{1,3,1},{3,2,0},{1,0,1}}-x)", "!");
		// Solver.deactivateCross();
	}

	@Test
	public void testEquations(){
		final Matrix A = matrixOrFail("{x,y}");
		SolverUtils.checkSolve(A, "Passaggi:\nx,y\nx=0,y=0");
	}

	@Test
	public void testExercise(){
		// in P^2(R)
		// P_1=[1,1,1] P_2=[1,1,0] P_3=[1,0,0]
		// Q=[1,2,3] Q'=[3,2,1]
		// f \in PGL(P^2(R)) :
		// f(P_i) = P_i \forall i = 1 .. 3
		// f(Q) = Q'

		// A := ( P_3 P_2 P_1)
		final Matrix A = matrixOrFail("{{1,1,1},{0,1,1},{0,0,1}}");
		// D := diag(1,x,u)
		final Matrix D = matrixOrFail("{{1,0,0},{0,x,0},{0,0,y}}");
		// M := A D A^-1
		final Matrix M = A.multiply(D).multiply(A.minverse());
		// M Q = z Q'
		final Expression Q = parseOrFail("tr{1,2,3}");
		final Expression Qp = parseOrFail("tr{3,2,1}");
		final Expression eq = new BooleanOp(new Multiplication(M, Q), '=', new Multiplication(Letter.Z, Qp));
		SolverUtils.checkSolve(eq, "Passaggi:\n{{1,-1+x,-x+y},{0,x,-x+y},{0,0,y}}(tr({1,2,3}))=z(tr({3,2,1}))\n"
				+ "{{1,-1+x,-x+y},{0,x,-x+y},{0,0,y}}(tr({1,2,3}))-z(tr({3,2,1}))=0\n"
				+ "{{1,x-1,y-x},{0,x,y-x},{0,0,y}}{{1},{2},{3}}-z{{3},{2},{1}}=0\n"
				+ "{{1+x*2-2+y*3-x*3},{x*2+y*3-x*3},{y*3}}+{{-3z},{-2z},{-z}}=0\n"
				+ "{{1+2x-2+3y-3x},{2x+3y-3x},{3y}}+{{-3z},{-2z},{-z}}=0\n"
				+ "{{-1+2x+3y-3x},{3y-x},{3y}}+{{-3z},{-2z},{-z}}=0\n"
				+ "{{2x-1+3y-3x},{3y-x},{3y}}+{{-3z},{-2z},{-z}}=0\n{{3y-x-1},{3y-x},{3y}}+{{-3z},{-2z},{-z}}=0\n"
				+ "{{3y-x-1-3z},{3y-x-2z},{3y-z}}=0\n3y-x-1-3z=0,3y-x-2z=0,3y-z=0\n-x+3y-1-3z=0,3y-x-2z=0,3y-z=0\n"
				+ "-x=1+3z-3y,3y-x-2z=0,3y-z=0\nx=3y-1-3z,3y-x-2z=0,3y-z=0\nx=3y-1-3z,3y-(3y-1-3z)-2z=0,3y-z=0\n"
				+ "x=3y-1-3z,3y-3y+1--3z-2z=0,3y-z=0\nx=3y-1-3z,1+3z-2z=0,3y-z=0\nx=3y-1-3z,z+1=0,3y-z=0\n"
				+ "x=3y-1-3z,z=-1,3y-z=0\nx=3y-1-3z,z=-1,3y+1=0\nx=3y-1-3z,z=-1,3y=-1\n"
				+ "x=3y-1-3z,z=-1,y=-1/3\nx=3*-1/3-1-3z,z=-1,y=-1/3\nx=3*-1/3-1--3,z=-1,y=-1/3\n"
				+ "x=-3/3-1+3,z=-1,y=-1/3\nx=-1-1+3,z=-1,y=-1/3\nx=1,z=-1,y=-1/3");

		INumber minusOneThird = Int.MINUSONE.divide(Int.THREE);
		ValueList valueList = new ValueList('x', Int.ONE, 'y', minusOneThird);
		valueList.add('z', Int.MINUSONE);
		final Expression sol = M.partialSubstitute(valueList).multiply(Int.THREE);
		assertSimplify(sol, "{{3,0,-4},{0,3,-4},{0,0,-1}}");
		final Matrix P1 = (Matrix)SimplificationHelper.simplify(parseOrFail("tr{1,1,1}"));
		assertSimplify(new Multiplication(sol, P1), "-1*tr{1,1,1}");
		final Matrix P2 = (Matrix)SimplificationHelper.simplify(parseOrFail("tr{1,1,0}"));
		assertSimplify(new Multiplication(sol, P2), "3*tr{1,1,0}");
		final Matrix P3 = (Matrix)SimplificationHelper.simplify(parseOrFail("tr{1,0,0}"));
		assertSimplify(new Multiplication(sol, P3), "3*tr{1,0,0}");

		assertSimplify(new Multiplication(sol, Q), "-3*tr{3,2,1}");
	}

	@Test
	public void testSubstitute(){
		final ISubstitutible expr = parseOrFail("traa");
		final Matrix matA = matrixOrFail("{{1},{2},{3}}");
		final Expression sub = expr.partialSubstitute('a', matA);
		final Object sim = SimplificationHelper.simplify(sub);
		assertEquals("Substitution error", "{14}", sim.toString());
	}

	@Test
	public void testParse(){
		final Matrix empty1 = matrixOrFail("{{}}");
		assertEquals("Empty1 parsing failed.", "Mat{}", empty1.toFullString());
		final Matrix empty2 = matrixOrFail("{}");
		assertEquals("Empty2 parsing failed.", "Mat{}", empty2.toFullString());
		final Matrix vector = matrixOrFail("{a,b,c,d}");
		assertEquals("Vector parsing failed.", "Mat{L(a),L(b),L(c),L(d)}", vector.toFullString());
	}

	@Test
	public void testToString(){
		final Int[][] vals = new Int[][] {
				{
						new Int(3), new Int(5)
				}, {
						new Int(1), new Int(2)
				}
		};
		final Matrix mat = new Matrix(vals);
		final String matString = mat.toString();
		assertEquals("toString() failed", "{{3,5},{1,2}}", matString);
	}

	@Test
	public void testMultiplication(){
		final Matrix matA = matrixOrFail("{{a,b},{c,d}}");
		final Matrix matB = matrixOrFail("{{e,f},{g,h}}");
		final Matrix matAB = matA.multiply(matB);
		assertEquals("Multiplication failed", "{{ae+bg,af+bh},{ce+dg,cf+dh}}", matAB.toString());
	}

	@Test
	public void inverseTest(){
		Matrix mat = matrixOrFail("{{3,5},{1,2}}");
		checkInverse(mat);
		mat = matrixOrFail("{{a,b},{c,0}}");
		checkInverse(mat);
		final Expression inv = parseOrFail("{{a,b,c}}^-1");
		try{
			SimplificationHelper.simplify(inv);
			fail("InversionException not thrown");
		}
		catch(final InversionException e){

		}
		inv.value();
		inv.dvalue();
	}

	private static void checkInverse(final Matrix mat){
		final ICalculable det = MatrixMath.det(mat);
		if(det == null){
			MatrixMath.det(mat);
			fail("Null det");
			return;
		}
		if(det.isZero()) // singular matrix
			return;
		final Expression inv = mat.inverse();
		final Expression sim = SimplificationHelper.simplify(inv);
		Expression identity = mat.multiply(sim);
		assertTrue("A*inverse wasn't 1", SimplificationHelper.simplify(identity).isOne());
		identity = sim.multiply(mat);
		assertTrue("inverse*A wasn't 1", SimplificationHelper.simplify(identity).isOne());
	}

	@Test
	public void testRandomInverse(){
		final Random rand = new Random(0);
		for(int c = 0; c < 100; c++){
			final Int[][] vals = new Int[3][3];
			for(int i = 0; i < 3; i++)
				for(int j = 0; j < 3; j++)
					vals[i][j] = new Int(rand.nextInt(10) - 5);
			checkInverse(new Matrix(vals));
		}
	}

	@Test
	public void testOpposite(){
		final Matrix matA = matrixOrFail("{{a,b},{c,d}}");
		final Matrix matB = matrixOrFail("{{x,y},{z,t}}");
		final ISubstitutible matC = parseOrFail("A*B-B*A");
		final Object matD = SimplificationHelper.simplify(matC.partialSubstitute(new ValueList('A', matA, 'B', matB)));
		assertEquals("Commutator test failed:", "{{bz-cy,ay+bt-bx-dy},{cx+dz-az-ct,cy-bz}}", matD.toString());
	}

	@Test
	public void testTranspose(){
		checkTranspose("{{a,b},{e,i}}", "{{a,e},{b,i}}");
		checkTranspose("{a,b,c,d}", "{{a},{b},{c},{d}}");
		final Expression base1 = parseOrFail("tr{1,0,0}");
		final Object baseSim = SimplificationHelper.simplify(base1);
		assertEquals("tr{1,0,0}!={{1},{0},{0}}", "{{1},{0},{0}}", baseSim.toString());

		final IValue tra = parseOrFail("tra");
		tra.value();
	}

	@Test
	public void subTest(){
		final Expression matdiv = parseOrFail("{-2,1,1}/sqrt6");
		SimplificationHelper.simplify(matdiv);
		setLogToNormal();
		checkSubForm("{{1,0,0}}");
		setLogToFail();
	}

	@Test
	public void testValue(){
		final Expression test = parseOrFail("tr{a,b}{c,d}");
		final INumber val = test.value();
		assertEquals(val.toFullString(), "Cx(Int(0),Int(0))");
	}

	private static void checkSubForm(final String input){
		final Expression parsed = parseOrFail(input);
		final IOutputtable inSim = SimplificationHelper.simplify(parsed);
		assertEquals(input + " simplifies to what?", parsed.toFullString(), inSim.toFullString());
		parsed.value();
	}

	private static void checkTranspose(final String matrix, final String target){
		final Matrix matA = matrixOrFail(matrix);
		final Matrix trA = MatrixMath.transpose(matA);
		assertEquals("Transpose failed.", target, trA.toString());
		final Matrix ttA = MatrixMath.transpose(trA);
		assertEquals("trtrA != A", matA, ttA);
	}

	@Test
	public void testDeterminant(){
		final Matrix matA = matrixOrFail("{{a,b},{c,d}}");
		final Object det = SimplificationHelper.simplify(MatrixMath.det(matA));
		assertEquals("Determinant fail.", "ad-bc", Cleaner.clean(det.toString()));

		final Matrix matSingle = matrixOrFail("{a}");
		final Object detSingle = SimplificationHelper.simplify(MatrixMath.det(matSingle));
		assertEquals("Determinant fail.", "a", detSingle.toString());
		final Matrix vector = matrixOrFail("{-2,1,1}");
		try{
			MatrixMath.det(vector);
			fail("Determinant fail for vector.");
		}
		catch(final DeterminantException e){
		}

		final IValue det1 = parseOrFail("det1");
		det1.dvalue();
	}

	@Test
	public void testRank(){
		final Matrix matA = matrixOrFail("{{1,1},{0,0}}");
		final ISubstitutible rank = parseOrFail("rkA");
		final IValue subsA = rank.partialSubstitute('A', matA);
		final double val = subsA.dvalue();
		assertEquals("Rank incorrect.", 1, val, 0);

		final Matrix matB = matrixOrFail("{{1,0},{0,1}}");
		final IValue subsB = rank.partialSubstitute('A', matB);
		final double valb = subsB.dvalue();
		assertEquals("Rank incorrect.", 2, valb, 0);

		final Matrix matC = matrixOrFail("{{a,b},{1,0},{2a,2b}}");
		final IValue subsC = rank.partialSubstitute('A', matC);
		final double valc = subsC.dvalue();
		assertEquals("Rank incorrect.", 2, valc, 0);
	}

	@Test
	public void testStrange(){
		final Expression expr = parseOrFail("b{{a},{b},{c}}tr{{a},{b},{c}}");
		SimplificationHelper.simplify(expr);
	}

	@Test
	public void testSub(){
		final Matrix matA = matrixOrFail("{a,b,c}");
		final Object subA = matA.partialSubstitute('a', Letter.E);
		assertEquals("Substitution not working for vector.", "{e,b,c}", subA.toString());
	}
}
