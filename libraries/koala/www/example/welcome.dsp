<%dsp:taglib name="example" prefix="ex"/>

<html>
<head>
  <title>DSP Example -- Welcome</title>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <h2>Welcome, <ex:current-username/>!</h2>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>

