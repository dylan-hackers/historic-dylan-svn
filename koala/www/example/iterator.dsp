<%dsp:taglib name="example" prefix="ex"/>

<html>
<head>
  <title>DSP Example -- Home</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <ex:show-errors/>

  <h2>Iterator</h2>

  This page demonstrates a simple iterator tag called &quot;repeat&quot;.  It's really
  nothing more than a tag that specifies the &quot;body&quot; modifier.

  <p>Specify a query value of n=xxx in the URL to change the number of iterations.
  <p>
  <ex:repeat>
    <br>This is iteration <ex:display-iteration-number/>.
  </ex:repeat>


  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
