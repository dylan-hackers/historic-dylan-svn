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
  on one to see what happens and then look at the Dylan source (koala-example.dylan)
  to see how it's implemented.  There are comments in the source that
  should be helpful.

  <p>
  <h3>Contents</h3>

  <ol>
    <li><a href="login.dsp">Login</a></li>
    <li><a href="logout.dsp">Logout</a></li>
    <li><a href="iterator.dsp?n=3">Iterator</a></li>
    <li><a href="login.dsp">Login</a></li>
    <li><a href="login.dsp">Login</a></li>
    <li><a href="login.dsp">Login</a></li>
    <li><a href="login.dsp">Login</a></li>
  </ol>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
