<%dsp:taglib name="demo"/>

<html>
<head>
  <title>DSP Example -- Welcome</title>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <h2>Welcome, <demo:current-username/>!</h2>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>

