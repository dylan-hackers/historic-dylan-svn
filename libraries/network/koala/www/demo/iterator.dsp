<%dsp:taglib name="demo"/>

<html>
<head>
  <title>DSP Example -- Iterator</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <demo:show-errors/>

  <h2>Iterator</h2>

  This page demonstrates a simple iterator tag called &quot;repeat&quot;.  It's really
  nothing more than a tag that specifies the &quot;body&quot; modifier.

  <p>Specify a query value of n=xxx in the URL to change the number of iterations.
  <p>
  <demo:repeat>
    <br>This is iteration <demo:display-iteration-number/>.
  </demo:repeat>


  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
