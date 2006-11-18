<html>
<head>
  <title>DSP Example -- Home</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <h2>Home</h2>

  Each page of this demo demonstrates at least one different feature of
  Dylan Server Pages.  There's no preset order to the pages; just click
  on one to see what happens and then look at the Dylan source (koala-basics.dylan)
  to see how it's implemented.  There are comments in the source that
  should be helpful.

  <p>
  <h3>Contents</h3>

  <ol>
    <li><a href="hello.dsp">Hello World</a></li>
    <li><a href="args.dsp">A tag with arguments</a></li>
    <li><a href="login.dsp">Login (demonstrates sessions)</a></li>
    <li><a href="logout.dsp">Logout (demonstrates sessions)</a></li>
    <li><a href="iterator.dsp?n=3">Iterator (demonstrates query values and body tags)</a></li>
    <li><a href="table.dsp">Table Generation</a></li>
  </ol>

  <p>
  <h3>Low-level Koala API</h3>
  
  <ol>
    <li><a href="/responder1">A responder (the most basic way to respond to a URL)</a></li>
    <li><a href="/hello?a=1&b=2">Hello World (a non-DSP page)</a></li>
  </ol>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
