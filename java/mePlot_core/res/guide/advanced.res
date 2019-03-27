<h1>Advanced tutorial</h1>

<h2>Index</h2>
<ul>
  <li><a href="#substitution">Substitution</a></li>
  <li><a href="#multiple_substitution">Multiple substitution</a></li>
</ul>

<h2><a name="substitution">Substitution</a></h2>
<h3>Syntax</h3>
<pre>
[xY]Z
</pre>
or (equivalent)
<pre>
[x=Y]Z
</pre>
<ul>
  <li><b>x</b>: Any letter</li>
  <li><b>Y</b>: Any expression</li>
  <li><b>Z</b>: Any expression</li>
</ul>

<h3>Meaning</h3>
Z, in which x is replaced with Y. The substitution occurs after parsing, so:
<pre>
[xa+b]xx
</pre>
is parsed as
<pre>
(a+b)(a+b)
</pre>

<h2><a name="multiple_substitution">Multiple substitution</a></h2>
<h3>Syntax</h3>
Syntax for 2 simultaneous substitutions, works for any number of them.
<pre>
[x=Y;z=W]K
</pre>
or (equivalent)
<pre>
[xY;zW]K
</pre>

<h3>Meaning</h3>
K, in which x is replaced with Y, and z with W. The substitutions occur,
simultaneously, after parsing, so:
<pre>
[xa+1;ac]xa
</pre>
is parsed as
<pre>
(a+1)c
</pre>

You can chain substitution instead like this:
<pre>
[ca][xc+1]x
</pre>
is parsed as
<pre>
a+1
</pre>