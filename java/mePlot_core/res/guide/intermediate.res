<h1>Intermediate tutorial</h1>

<h2>Index</h2>
<ul>
  <li><a href="#types">Graph types</a></li>
  <ul>
    <li><a href="#simple2d">Simple 2D</a></li>
    <li><a href="#radial2d">Radial 2D</a></li>
    <li><a href="#parametric2d">Parametric 2D</a></li>
    <li><a href="#implicit2d">Implicit 2D</a></li>
  </ul>
</ul>

<h2><a name="substitution">Graph types</a></h2>
MePlot automatically detects the type of graph to draw,
inspecting the input for a particular form.

<h3><a name="simple2d">Simple 2D</a></h3>
<h4>Syntax</h4>
<pre>
y=X
</pre>
or (equivalent)
<pre>
X
</pre>
<ul>
  <li><b>X</b>: Any expression containing the letter x</li>
</ul>

<h4>Examples</h4>
<ul>
  <li>y=xx</li>
  <li>y=sinx</li>
  <li>x+1</li>
</ul>

<h3><a name="radial2d">Radial 2D</a></h3>
<h4>Syntax</h4>
<pre>
r=T
</pre>
<ul>
  <li><b>T</b>: Any expression containing the letter t</li>
</ul>

<h4>Examples</h4>
<ul>
  <li>r=3sin(4t)</li>
  <li>r=t/2</li>
</ul>

<h3><a name="parametric2d">Parametric 2D</a></h3>
<h4>Syntax</h4>
<pre>
x=T1;y=T2
</pre>
or (equivalent)
<pre>
y=T2;x=T1
</pre>
or (equivalent)
<pre>
T1;T2
</pre>
<ul>
  <li><b>T1,T2</b>: Any expression containing the letter t</li>
</ul>

<h4>Examples</h4>
<ul>
  <li>r

<h3><a name="implicit2d">Implicit 2D</a></h3>
An implicit 2D graph is anything of the form:
<pre>
XY1=XY2
</pre>
<ul>
  <li><b>XY1,XY2</b>: Any expression containing the letter x and/or y</li>