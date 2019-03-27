package test.solver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.expressions.IOutputtable;
import meplot.expressions.Letter;
import meplot.expressions.other.Poly;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.solver.AbstractSolver;

import org.junit.Test;

public final class SolverTest extends SolverUtils {
	@Test
	public void testAbs() {
		checkSolve("absx<0", "Passaggi:\nabs(x)<0\n∄", true);
		checkSolve("absx<=0", "Passaggi:\nabs(x)≤0\nx=0", true);
		checkSolve("absx=0", "Passaggi:\nabs(x)=0\nx=0", true);
		checkSolve("absx>=0", "Passaggi:\nabs(x)≥0\n∀", true);
		checkSolve("absx>0", "Passaggi:\nabs(x)>0\nx≠0", true);
		checkSolve("absx<>0", "Passaggi:\nabs(x)≠0\nx≠0", true);

		checkSolve("absx<2", "Passaggi:\nabs(x)<2\n-2<x,x<2", true);
		checkSolve("absx<=2", "Passaggi:\nabs(x)≤2\n-2≤x,x≤2", true);
		checkSolve("absx=2", "Passaggi:\nabs(x)=2\n°x=2\n°x=-2", true);
		checkSolve("absx>=2", "Passaggi:\nabs(x)≥2\n°x≥2\n°x≤-2", true);
		checkSolve("absx>2", "Passaggi:\nabs(x)>2\n°x>2\n°x<-2", true);
		checkSolve("absx<>2", "Passaggi:\nabs(x)≠2\nx≠-2,x≠2", true);

		checkSolve("absx<-2", "Passaggi:\nabs(x)<-2\n∄", true);
		checkSolve("absx<=-2", "Passaggi:\nabs(x)≤-2\n∄", true);
		checkSolve("absx=-2", "Passaggi:\nabs(x)=-2\n∄", true);
		checkSolve("absx>=-2", "Passaggi:\nabs(x)≥-2\n∀", true);
		checkSolve("absx>-2", "Passaggi:\nabs(x)>-2\n∀", true);
		checkSolve("absx<>-2", "Passaggi:\nabs(x)≠-2\n∀", true);
	}

	@Test
	public void testBig() {
		assertSimplify("c((bz)/c)-bz", "0");
		checkLeaves("[a{{a,b},{c,d}};b{{x,y},{z,w}}]ab-ba=0", "y=(bz)/c,w=(cx+dz-az)/c");
	}

	@Test
	public void testCollect() {
		assertSimplify("y-2y", "-y");
	}

	@Test
	public void testDerivatives() {
		checkSolve("dd180xxx,x", "Passaggi:\ndd(180xxx,x)\ndd(180xxx,x)=0\ndd(180xx^2,x)=0\ndd(180x^3,x)=0\n"
				+ "x^3dd(180,x)+180dd(x^3,x)=0\n0x^3+180*3x^2=0\n540x^2=0\nx=0");
	}

	@Test
	public void testExample() {
		checkHtmlSolve("x+y=10;x-yy=2", "Passaggi:<br/>\n"
				+ "$\\{\\table {{{x}+{y}}={10}}; {{{x}{-({{y}{y}})}}={2}}$<br/>\n"
				+ "$\\{\\table {{{x}+{y}}={10}}; {{{x}{-{{y}^{2}}}}={2}}$<br/>\n"
				+ "$\\{\\table {{{{x}+{y}}{-10}}={0}}; {{{x}{-{{y}^{2}}}}={2}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{{x}{-{{y}^{2}}}}={2}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{{{10}{-{y}}}{-{{y}^{2}}}}={2}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{{{10}{-{y}}{-{{y}^{2}}}}{-2}}={0}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{{{8}{-{y}}}{-{{y}^{2}}}}={0}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{{{-{{y}^{2}}}{-{y}}}+{8}}={0}}$<br/>\n"
				+ "<table><tr><td><div class=\"border\">\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}+{√{{(-1)^{2}}+{32}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}+{√{{{1}^{2}}+{32}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}+{√{{1}+{32}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}+{√{33}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{-1}{-{√{33}}}}/{2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{-1}/{2}}{{{-1}/{2}}{√{33}}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-({{{-1}/{2}}{{{-1}/{2}}{√{33}}}})}}};"
				+ " {{y}={{{-1}/{2}}{{{-1}/{2}}{√{33}}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}+{{1}/{2}}{-{{-1}/{2}}{√{33}}}}};"
				+ " {{y}={{{-1}/{2}}{{{-1}/{2}}{√{33}}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{{21}/{2}}+{{{1}/{2}}{√{33}}}}}; {{y}={{{-1}/{2}}{{{-1}/{2}}{√{33}}}}}$<br/>\n"
				+ "</div>\n</td><td><div class=\"border\">$\\{\\table {{x}={{10}{-{y}}}};"
				+ " {{y}={{{1}{-{√{{(-1)^{2}}+{32}}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}{-{√{{{1}^{2}}+{32}}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}{-{√{{1}+{32}}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{1}{-{√{33}}}}/{-2}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{-1}/{2}}+{{{1}/{2}}{√{33}}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{y}}}}; {{y}={{{{1}/{2}}{√{33}}}{{-1}/{2}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-({{{{1}/{2}}{√{33}}}{{-1}/{2}}})}}};"
				+ " {{y}={{{{1}/{2}}{√{33}}}{{-1}/{2}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{10}{-{{1}/{2}}{√{33}}}+{{1}/{2}}}}; {{y}={{{{1}/{2}}{√{33}}}{{-1}/{2}}}}$<br/>\n"
				+ "$\\{\\table {{x}={{{21}/{2}}{{{-1}/{2}}{√{33}}}}}; {{y}={{{{1}/{2}}{√{33}}}{{-1}/{2}}}}$<br/>\n"
				+ "</div>\n</td></tr></table>");
	}

	@Test
	public void testErrors() {
		checkSolve("x", "Passaggi:\nx\nx=0");
		checkSolve("a", "Passaggi:\na\na=0");
		checkSolve("0x=0", "Passaggi:\n0x=0\n0=0\n∀");
		checkSolve("0x=3", "Passaggi:\n0x=3\n0x-3=0\n-3=0\n∄");
		checkSolve("1", "Passaggi:\n1");
		checkSolve("x=2; x=3", "Passaggi:\nx=2,x=3\nx=2,2=3\nx=2,∄\n∄");
	}

	@Test
	public void testFold() {
		final Expression l = new Letter('a');
		final ExpressionTree tree = new ExpressionTree(l);
		ExpressionTree leaf = tree;
		for (int i = 0; i < 5; i++)
			leaf = leaf.addChild(l);
		final ExpressionTree tree2 = tree.stringFold();
		assertEquals("a", tree2.toString());
	}

	@Test
	public void testFraction() {
		checkSolve("(xx-1)/(x+1)",
				"Passaggi:\n(xx-1)/(x+1)\n(xx-1)/(x+1)=0\n" + "(x^2-1)/(x+1)=0\n(x-1)/1=0\nx-1=0\nx=1");
	}

	@Test
	public void testInequalities1() {
		checkSolve("-2x>-5", "Passaggi:\n-(2x)>-5\n-(2x)+5>0\n-2x+5>0\n-2x>-5\nx<5/2", true);
		checkSolve("x^2>3", "Passaggi:\nx^2>3\n°x>sqrt(3)\n°x<-sqrt(3)", true);
		checkSolve("x^2<3", "Passaggi:\nx^2<3\n-sqrt(3)<x,x<sqrt(3)", true);
		checkSolve("x>=2xx", "Passaggi:\nx≥2xx\nx-2xx≥0\nx-2x^2≥0\n-2x^2+x≥0\n-2/2≤x,x≤(-1+sqrt(1^2))/2\n"
				+ "-2/2≤x,x≤(-1+sqrt(1))/2\n-1≤x,x≤(-1+1)/2\n-1≤x,x≤0/2\n-1≤x,x≤0", true);
	}

	@Test
	public void testInequalities2() {
		checkSolve("x^2-x>0",
				"Passaggi:\nx^2-x>0\n°x>(1+sqrt((-1)^2))/2\n|x>(1+sqrt(1^2))/2\n"
						+ "|x>(1+sqrt(1))/2\n|x>(1+1)/2\n|x>2/2\n|x>1\n°x<(1-sqrt((-1)^2))/2\n"
						+ "|x<(1-sqrt(1^2))/2\n|x<(1-sqrt(1))/2\n|x<(1-1)/2\n|x<0/2\n|x<0",
				true);
		checkSolve("x^2-x>=0",
				"Passaggi:\nx^2-x≥0\n°x≥(1+sqrt((-1)^2))/2\n|x≥(1+sqrt(1^2))/2\n"
						+ "|x≥(1+sqrt(1))/2\n|x≥(1+1)/2\n|x≥2/2\n|x≥1\n°x≤(1-sqrt((-1)^2))/2\n"
						+ "|x≤(1-sqrt(1^2))/2\n|x≤(1-sqrt(1))/2\n|x≤(1-1)/2\n|x≤0/2\n|x≤0",
				true);
		checkSolve("-x^2+x<0", "Passaggi:\n-x^2+x<0\n°x>(-1+sqrt(1^2))/2\n|x>(-1+sqrt(1))/2\n"
				+ "|x>(-1+1)/2\n|x>0/2\n|x>0\n°x<-2/2\n|x<-1", true);
		checkSolve("-x^2+x<=0", "Passaggi:\n-x^2+x≤0\n°x≥(-1+sqrt(1^2))/2\n|x≥(-1+sqrt(1))/2\n"
				+ "|x≥(-1+1)/2\n|x≥0/2\n|x≥0\n°x≤-2/2\n|x≤-1", true);
		checkSolve("-x^2+x>0", "Passaggi:\n-x^2+x>0\n-2/2<x,x<(-1+sqrt(1^2))/2\n"
				+ "-2/2<x,x<(-1+sqrt(1))/2\n-1<x,x<(-1+1)/2\n-1<x,x<0/2\n-1<x,x<0", true);
		checkSolve("-x^2+x>=0", "Passaggi:\n-x^2+x≥0\n-2/2≤x,x≤(-1+sqrt(1^2))/2\n"
				+ "-2/2≤x,x≤(-1+sqrt(1))/2\n-1≤x,x≤(-1+1)/2\n-1≤x,x≤0/2\n-1≤x,x≤0", true);
		checkSolve("x^2-x<0",
				"Passaggi:\nx^2-x<0\n(1-sqrt((-1)^2))/2<x,x<(1+sqrt((-1)^2))/2\n"
						+ "(1-sqrt(1^2))/2<x,x<(1+sqrt(1^2))/2\n(1-sqrt(1))/2<x,x<(1+sqrt(1))/2\n"
						+ "(1-1)/2<x,x<(1+1)/2\n0/2<x,x<2/2\n0<x,x<1",
				true);
		checkSolve("x^2-x<=0",
				"Passaggi:\nx^2-x≤0\n(1-sqrt((-1)^2))/2≤x,x≤(1+sqrt((-1)^2))/2\n"
						+ "(1-sqrt(1^2))/2≤x,x≤(1+sqrt(1^2))/2\n(1-sqrt(1))/2≤x,x≤(1+sqrt(1))/2\n"
						+ "(1-1)/2≤x,x≤(1+1)/2\n0/2≤x,x≤2/2\n0≤x,x≤1",
				true);
	}

	@Test
	public void testLetter() {
		checkSolve("y^2=3", "Passaggi:\ny^2=3\n°y=sqrt(3)\n°y=-sqrt(3)");
		checkSolve("3a=1", "Passaggi:\n3a=1\na=1/3");
		checkSolve("3b=1", "Passaggi:\n3b=1\nb=1/3");
		checkSolve("3w=1", "Passaggi:\n3w=1\nw=1/3");
		checkSolve("3y=1", "Passaggi:\n3y=1\ny=1/3");
		checkSolve("3z=1", "Passaggi:\n3z=1\nz=1/3");
	}

	@Test
	public void testMatrix() {
		checkLeaves("[a{{a,b},{c,d}}]aa-(x+ya)", "x=bc-ad,y=a+d");
	}

	@Test
	public void testOpen() {
		checkSolve("(x+2)/2=0", "Passaggi:\n(x+2)/2=0\nx+2=0\nx=-2");
		checkSolve("(x+2)*2=0", "Passaggi:\n(x+2)*2=0\nx*2+4=0\n2x+4=0\n2x=-4\nx=-2");
		checkSolve("x/2+1/3=0", "Passaggi:\nx/2+1/3=0\n(x*3+2)/6=0\n1/2x+1/3=0\n1/2x=-1/3\nx=-2/3");
	}

	@Test
	public void testParametric() {
		setLogToNormal();
		checkSolve("ax+y=b;x+y=q",
				"Passaggi:\nax+y=b,x+y=q\nax+y-b=0,x+y=q\nax=b-y,x+y=q\n"
						+ "x=(b-y)/a,x+y=q\nx=(b-y)/a,(b-y)/a+y=q\nx=(b-y)/a,(b-y+ay)/a=q\n"
						+ "x=(b-y)/a,(b-y+ay-qa)/a=0\nx=(b-y)/a,(b-y+ay-aq)/a=0\nx=(b-y)/a,b-y+ay-aq=0\n"
						+ "x=(b-y)/a,y(a-1)+b-aq=0\nx=(b-y)/a,y(a-1)=aq-b\nx=(b-y)/a,y=(aq-b)/(a-1)\n"
						+ "x=(b-((aq-b)/(a-1)))/a,y=(aq-b)/(a-1)\nx=(b+(-aq+b)/(a-1))/a,y=(aq-b)/(a-1)\n"
						+ "x=(b-aq+(a-1)b)/(a-1)/a,y=(aq-b)/(a-1)\nx=(b-aq+ba-b)/(a-1)/a,y=(aq-b)/(a-1)\n"
						+ "x=(b-aq+ab-b)/(a-1)/a,y=(aq-b)/(a-1)\nx=(ab-aq)/(a-1)/a,y=(aq-b)/(a-1)\n"
						+ "x=(ab-aq)/((a-1)a),y=(aq-b)/(a-1)\nx=b-q/a-1,y=(aq-b)/(a-1)\nx=(b-q)/(a-1),y=(aq-b)/(a-1)");
		setLogToFail();
	}

	@Test
	public void testParsing() {
		checkSolve("x=0.8@5", "Passaggi:\nx=0.8@5\nx-77/90=0\nx=77/90");
		checkSolve("x=0.@3", "Passaggi:\nx=0.@3\nx-3/9=0\nx-1/3=0\nx=1/3");
		checkSolve("x=0@3", "Passaggi:\nx=0@3\nx-3/9=0\nx-1/3=0\nx=1/3");
		checkSolve(".5x=0", "Passaggi:\n.5x=0\n1/2x=0\nx=0");
		checkSolve("x=0.5", "Passaggi:\nx=0.5\nx-5/10=0\nx-1/2=0\nx=1/2");
		checkSolve("3x=.5", "Passaggi:\n3x=.5\n3x-5/10=0\n3x-1/2=0\n3x=1/2\nx=1/6");
		checkSolve("x=8@5", "Passaggi:\nx=8@5\nx-77/9=0\nx=77/9");
	}

	@Test
	public void testPoly() {
		checkSolve("ax+b=0", "Passaggi:\nax+b=0\nax=-b\nx=(-b)/a");
		checkSolve("x^2=1", "Passaggi:\nx^2=1\n°x=1\n°x=-1");
		checkSolve("xx=y", "Passaggi:\nxx=y\nxx-y=0\nx^2-y=0\nx^2=y\n°x=sqrt(y)\n°x=-sqrt(y)");
		checkSolve("y=xx", "Passaggi:\ny=xx\ny-xx=0\ny-x^2=0\n-x^2+y=0\n-x^2=-y\n°x=sqrt(y)\n°x=-sqrt(y)");
		checkSolve("xx-5x+6=0",
				"Passaggi:\nxx-(5x)+6=0\nx^2-5x+6=0\n" + "°x=(5+sqrt((-5)^2-24))/2\n|x=(5+sqrt(5^2-24))/2\n"
						+ "|x=(5+sqrt(25-24))/2\n|x=(5+sqrt(1))/2\n|x=(5+1)/2\n|x=6/2\n|x=3\n"
						+ "°x=(5-sqrt((-5)^2-24))/2\n|x=(5-sqrt(5^2-24))/2\n"
						+ "|x=(5-sqrt(25-24))/2\n|x=(5-sqrt(1))/2\n|x=(5-1)/2\n|x=4/2\n|x=2");
		checkSolve("xx=1/2+1/3",
				"Passaggi:\nxx=1/2+1/3\nxx-1/2-1/3=0\n" + "x^2-5/6=0\nx^2=5/6\n°x=sqrt(30)/6\n°x=-1/6sqrt(30)");
		checkSolve("xx-3x^2+5x=@5",
				"Passaggi:\nxx-(3x^2)+5x=@5\nxx-(3x^2)+5x-5/9=0\n"
						+ "x^2-3x^2+5x-5/9=0\nx^2*-2+5x-5/9=0\n5x-2x^2-5/9=0\n-2x^2+5x-5/9=0\n"
						+ "°x=(-5+sqrt(5^2-40/9))/-4\n|x=(-5+sqrt(25-40/9))/-4\n|x=(-5+sqrt(185/9))/-4\n"
						+ "|x=(-5+sqrt(185)/sqrt(9))/-4\n|x=(-5+sqrt(185)/3)/-4\n|x=(sqrt(185)-15)/3/-4\n"
						+ "|x=(sqrt(185)-15)/-12\n|x=(-sqrt(185)+15)/12\n|x=5/4-1/12sqrt(185)\n"
						+ "°x=(-5-sqrt(5^2-40/9))/-4\n|x=(-5-sqrt(25-40/9))/-4\n|x=(-5-sqrt(185/9))/-4\n"
						+ "|x=(-5-(sqrt(185)/sqrt(9)))/-4\n|x=(-5-(sqrt(185)/3))/-4\n|x=(-5+(-sqrt(185))/3)/-4\n"
						+ "|x=(-5-1/3sqrt(185))/-4\n|x=5/4+1/12sqrt(185)");
		checkSolve("xxx/x=0", "Passaggi:\n(xxx)/x=0\nx^3/x=0\nx^2=0\nx=0");
		checkSolve("xx=2", "Passaggi:\nxx=2\nxx-2=0\nx^2-2=0\nx^2=2\n°x=sqrt(2)\n°x=-sqrt(2)");
	}

	@Test
	public void testPolyCoeff() {
		Expression expr = parseOrFail("d(-by+c)+aey-af");
		expr = SimplificationHelper.simplify(expr.expand());
		if (!Poly.isPoly(expr, 'y'))
			fail("d(-by+c)+aey-af is not an y-poly");
		final IOutputtable coeff = new Poly(expr, 'y').getCoefficent(1);
		assertEquals("-bd+ae", coeff.toCleanString());
	}

	@Test
	public void testSchool() {
		checkSolve("x-1=1/4y;2x+y=-1",
				"Passaggi:\nx-1=1/4y,2x+y=-1\nx-1-1/4y=0,2x+y=-1\n"
						+ "x=1+1/4y,2x+y=-1\nx=1+1/4y,2(1+1/4y)+y=-1\nx=1+1/4y,2+1/4y*2+y=-1\n"
						+ "x=1+1/4y,y*6/4+2=-1\nx=1+1/4y,3/2y+2=-1\nx=1+1/4y,3/2y+2+1=0\nx=1+1/4y,3+3/2y=0\n"
						+ "x=1+1/4y,3/2y+3=0\nx=1+1/4y,3/2y=-3\nx=1+1/4y,y=-2\nx=1+1/4*-2,y=-2\n"
						+ "x=1-2/4,y=-2\nx=1-1/2,y=-2\nx=1/2,y=-2");
		setLogToNormal();
		checkSolve("(x-y)/(x+4)=2;(x+5)/(y+3)=-1",
				"Passaggi:\n(x-y)/(x+4)=2,(x+5)/(y+3)=-1\n"
						+ "(x-y+x*-2-8)/(x+4)=0,(x+5)/(y+3)=-1\n(-x-y-8)/(x+4)=0,(x+5)/(y+3)=-1\n"
						+ "-x-y-8=0,(x+5)/(y+3)=-1\n-x=y+8,(x+5)/(y+3)=-1\nx=-y-8,(x+5)/(y+3)=-1\n"
						+ "x=-y-8,(-y-8+5)/(y+3)=-1\nx=-y-8,(-3-y)/(y+3)=-1\nx=-y-8,-1=-1\nx=-y-8,∀\nx=-y-8");
		setLogToFail();
		checkSolve("(2y-1)/x=(1+2x)/(3x);4/(x+y)=2/3",
				"Passaggi:\n(2y-1)/x=(1+2x)/(3x),4/(x+y)=2/3\n"
						+ "(2y-1)/x=1/3+2/3x/x,4/(x+y)=2/3\n(2y-1)/x=(1/3+2/3x)/x,4/(x+y)=2/3\n"
						+ "(2y-1-1/3-2/3x)/x=0,4/(x+y)=2/3\n(-4/3+2y-2/3x)/x=0,4/(x+y)=2/3\n"
						+ "(2y-4/3-2/3x)/x=0,4/(x+y)=2/3\n2y-4/3-2/3x=0,4/(x+y)=2/3\n-2/3x+2y-4/3=0,4/(x+y)=2/3\n"
						+ "-2/3x=4/3-2y,4/(x+y)=2/3\nx=3y-2,4/(x+y)=2/3\nx=3y-2,4/(3y-2+y)=2/3\n"
						+ "x=3y-2,4/(y*4-2)=2/3\nx=3y-2,4/(4y-2)=2/3\nx=3y-2,(12+4y*-2+4)/(4y*3-6)=0\n"
						+ "x=3y-2,(12+4-8y)/(12y-6)=0\nx=3y-2,(16-8y)/(12y-6)=0\nx=3y-2,(4/3-2/3y)/(y-1/2)=0\n"
						+ "x=3y-2,4/3-2/3y=0\nx=3y-2,-2/3y+4/3=0\nx=3y-2,-2/3y=-4/3\nx=3y-2,y=2\n"
						+ "x=3*2-2,y=2\nx=6-2,y=2\nx=4,y=2");

		checkSolve("3x+2y=1;x+y=0",
				"Passaggi:\n3x+2y=1,x+y=0\n3x+2y-1=0,x+y=0\n3x=1-2y,x+y=0\n"
						+ "x=1/3-2/3y,x+y=0\nx=1/3-2/3y,1/3-2/3y+y=0\nx=1/3-2/3y,y*1/3+1/3=0\n"
						+ "x=1/3-2/3y,1/3y+1/3=0\nx=1/3-2/3y,1/3y=-1/3\nx=1/3-2/3y,y=-1\nx=1/3--2/3,y=-1\n"
						+ "x=1/3+2/3,y=-1\nx=9/9,y=-1\nx=1,y=-1");
	}

	@Test
	public void testSchool2() {
		if (true)
			return;
		// assertSimplify("34+5(-10-3z-y)-3z+y=0", "?", false);
		checkSolve("[ax;bz;cy]10+a+3b+c=0;34+5a-3b+c=0;(-a/2)^2+(-b/2)^2-c=26", "Passaggi:\n"
				+ "10+x+3z+y=0,34+5x-(3z)+y=0,(-(x/2))^2+(-(z/2))^2-y=26\n"
				+ "10+x+3z+y=0,34+5x-3z+y=0,(-x)^2/2^2+(-z)^2/2^2-y=26\n"
				+ "10+x+3z+y=0,34+5x-3z+y=0,((-1)^2x^2)/4+((-1)^2z^2)/4-y=26\n"
				+ "10+x+3z+y=0,34+5x-3z+y=0,(1^2x^2)/4+(1^2z^2)/4-y=26\n"
				+ "10+x+3z+y=0,34+5x-3z+y=0,(1x^2)/4+(1z^2)/4-y=26\n10+x+3z+y=0,34+5x-3z+y=0,(x^2+z^2-y*4)/4=26\n"
				+ "10+x+3z+y=0,34+5x-3z+y=0,(x^2+z^2-4y)/4=26\nx+10+3z+y=0,34+5x-3z+y=0,(x^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,34+5x-3z+y=0,(x^2+z^2-4y)/4=26\n" + "x=-10-3z-y,34+5(-10-3z-y)-3z+y=0,(x^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,34+5(-10-3z-y)-3z+y=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,34-50-3z*5-y*5-3z+y=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,y-16-15z-5y-3z=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,y*-4-16-15z-3z=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,-4y-16-15z-3z=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,z*-18-4y-16=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,-18z-4y-16=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,-4y-18z-16=0,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,-4y=18z+16,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((-10-3z-y)^2+z^2-4y)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((-10-3z-(-9/2z-4))^2+z^2-4(-9/2z-4))/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((-10-3z--9/2z+4)^2+z^2-9/2z*-4+16)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((9/2z-6-3z)^2+z^2+36/2z+16)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((9/2z-6-3z)^2+z^2+18z+16)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((z*3/2-6)^2+z^2+18z+16)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((3/2z-6)^2+z^2+18z+16)/4=26\n"
				+ "x=-10-3z-y,y=-9/2z-4,((3/2z-6)^2+z^2+18z+16-104)/4=0\n"
				+ "x=-10-3z-y,y=-9/2z-4,(18z-88+(3/2z-6)^2+z^2)/4=0\n"
				+ "x=-10-3z-y,y=-9/2z-4,(18z+(3/2z-6)^2-88+z^2)/4=0\n"
				+ "x=-10-3z-y,y=-9/2z-4,18z+(3/2z-6)^2-88+z^2=0\n"
				+ "x=-10-3z-y,y=-9/2z-4,18z+(3/2z)^2+3/2z*-6*2+36-88+z^2=0\nx=-10-3z-y,y=-9/2z-4,13/4z^2-52=0\n"
				+ "x=-10-3z-y,y=-9/2z-4,13/4z^2=52\n°x=-10-3z-y,y=-9/2z-4,z=4\n|x=-10-3z-y,y=-9/2*4-4,z=4\n"
				+ "|x=-10-3*4-y,y=-9/2*4-4,z=4\n|x=-10-3*4-(-9/2*4-4),y=-9/2*4-4,z=4\n"
				+ "|x=-10-12--36/2--4,y=-9/2*4-4,z=4\n|x=-10-12--18+4,y=-9/2*4-4,z=4\n"
				+ "|x=-10-12+18+4,y=-9/2*4-4,z=4\n|x=0,y=-9/2*4-4,z=4\n|x=0,y=-36/2-4,z=4\n"
				+ "|x=0,y=-18-4,z=4\n|x=0,y=-22,z=4\n°x=-10-3z-y,y=-9/2z-4,z=-4\n"
				+ "|x=-10-3z-y,y=-9/2*-4-4,z=-4\n|x=-10-3*-4-y,y=-9/2*-4-4,z=-4\n"
				+ "|x=-10-3*-4-(-9/2*-4-4),y=-9/2*-4-4,z=-4\n|x=-10+12-36/2--4,y=-9/2*-4-4,z=-4\n"
				+ "|x=-10+12-18+4,y=-9/2*-4-4,z=-4\n|x=-12,y=-9/2*-4-4,z=-4\n|x=-12,y=36/2-4,z=-4\n"
				+ "|x=-12,y=18-4,z=-4\n|x=-12,y=14,z=-4");
		checkSolve("[ax;by;cz]10+a+3b+c=0;34+5a-3b+c=0;(-a/2)^2+(-b/2)^2-c=26", "!");
	}

	@Test
	public void testSqrt() {
		checkSolve("sqrtx=2", "Passaggi:\nsqrt(x)=2\nx=4,2≥0\nx=4,∀\nx=4", true);
		checkSolve("sqrtx=-2", "Passaggi:\nsqrt(x)=-2\nx=4,-2≥0\nx=4,∄\n∄", true);
	}

	@Test
	public void testStrange() {
		checkSolve("ysqrt(-yz)+wy=0",
				"Passaggi:\nysqrt(-(yz))+wy=0\n" + "ysqrt(-yz)+wy=0\nwy+ysqrt(-yz)=0\nwy=-ysqrt(-yz)\nw=-sqrt(-yz)");
	}

	@Test
	public void testSystem() {
		checkSolve("x=yy;y=sqrt(5)+1",
				"Passaggi:\nx=yy,y=sqrt(5)+1\n" + "x=y^2,y=sqrt(5)+1\nx=(sqrt(5)+1)^2,y=sqrt(5)+1\n"
						+ "x=5+2sqrt(5)+1,y=sqrt(5)+1\nx=6+2sqrt(5),y=sqrt(5)+1");
		checkSolve("x=2y;y=x-1", "Passaggi:\nx=2y,y=x-1\nx=2y,y=2y-1\nx=2y,y-2y+1=0\nx=2y,y+1-2y=0\nx=2y,1-y=0\n"
				+ "x=2y,-y+1=0\nx=2y,-y=-1\nx=2y,y=1\nx=2*1,y=1\nx=2,y=1");
		checkSolve("x=yy;y=x-1",
				"Passaggi:\nx=yy,y=x-1\nx=y^2,y=x-1\nx=y^2,y=y^2-1\n"
						+ "x=y^2,y-y^2+1=0\nx=y^2,y+1-y^2=0\nx=y^2,-y^2+y+1=0\n°x=y^2,y=(-1+sqrt(1^2+4))/-2\n"
						+ "|x=y^2,y=(-1+sqrt(1+4))/-2\n|x=y^2,y=(-1+sqrt(5))/-2\n|x=y^2,y=(-sqrt(5)+1)/2\n"
						+ "|x=y^2,y=1/2-1/2sqrt(5)\n|x=(1/2-1/2sqrt(5))^2,y=1/2-1/2sqrt(5)\n"
						+ "|x=1/4-1/2sqrt(5)*1/2*2+(-1/2sqrt(5))^2,y=1/2-1/2sqrt(5)\n"
						+ "|x=1/4-1/4*2sqrt(5)+(1/2)^2sqrt(5)^2,y=1/2-1/2sqrt(5)\n"
						+ "|x=1/4-2/4sqrt(5)+1/4abs(5),y=1/2-1/2sqrt(5)\n|x=1/4-1/2sqrt(5)+1/4*5,y=1/2-1/2sqrt(5)\n"
						+ "|x=1/4-1/2sqrt(5)+5/4,y=1/2-1/2sqrt(5)\n|x=24/16-1/2sqrt(5),y=1/2-1/2sqrt(5)\n"
						+ "|x=3/2-1/2sqrt(5),y=1/2-1/2sqrt(5)\n°x=y^2,y=(-1-sqrt(1^2+4))/-2\n"
						+ "|x=y^2,y=(-1-sqrt(1+4))/-2\n|x=y^2,y=(-1-sqrt(5))/-2\n|x=y^2,y=1/2+1/2sqrt(5)\n"
						+ "|x=(1/2+1/2sqrt(5))^2,y=1/2+1/2sqrt(5)\n"
						+ "|x=1/4+1/2sqrt(5)*1/2*2+(1/2sqrt(5))^2,y=1/2+1/2sqrt(5)\n"
						+ "|x=1/4+1/4*2sqrt(5)+1/4sqrt(5)^2,y=1/2+1/2sqrt(5)\n"
						+ "|x=1/4+2/4sqrt(5)+1/4abs(5),y=1/2+1/2sqrt(5)\n|x=1/4+1/2sqrt(5)+1/4*5,y=1/2+1/2sqrt(5)\n"
						+ "|x=1/4+1/2sqrt(5)+5/4,y=1/2+1/2sqrt(5)\n|x=24/16+1/2sqrt(5),y=1/2+1/2sqrt(5)\n"
						+ "|x=3/2+1/2sqrt(5),y=1/2+1/2sqrt(5)");
	}

	@Test
	public void testSystem2() {
		checkSolve("{xx=1,2y=x}", "Passaggi:\nxx=1,2y=x\nx^2=1,2y=x\n°x=1,2y=x\n|x=1,2y=1\n|x=1,y=1/2\n"
				+ "°x=-1,2y=x\n|x=-1,2y=-1\n|x=-1,y=-1/2");
		checkSolve("{{x,y},{z,w}", "Passaggi:\n{{x,y},{z,w}}\nx=0,y=0,z=0,w=0");
	}

	@Test
	public void testSystem3() {
		checkSolve("{x=2y,y=3x+4}",
				"Passaggi:\nx=2y,y=3x+4\nx=2y,y=3(2y)+4\nx=2y,y=3*2y+4\n"
						+ "x=2y,y=6y+4\nx=2y,y-6y-4=0\nx=2y,y*-5-4=0\nx=2y,-5y-4=0\nx=2y,-5y=4\n"
						+ "x=2y,y=-4/5\nx=2*-4/5,y=-4/5\nx=-8/5,y=-4/5");
	}

	@Test
	public void testSystem4() {
		// setLogToNormal();
		// AbstractSolver.activateCross();
		// checkLeaves("{ax+by=c,dx+ey=f}", "x=(ce-bf)/(ae-bd),y=(af-cd)/(ae-bd)");
		// AbstractSolver.deactivateCross();
		// setLogToFail();
	}

	@Test
	public void testTree() {
		checkSolve("xx=1;yy=2",
				"Passaggi:\nxx=1,yy=2\nx^2=1,y^2=2\n" + "°x=1,y^2=2\n|°x=1,y=sqrt(2)\n|°x=1,y=-sqrt(2)\n"
						+ "°x=-1,y^2=2\n|°x=-1,y=sqrt(2)\n|°x=-1,y=-sqrt(2)");
	}
}
