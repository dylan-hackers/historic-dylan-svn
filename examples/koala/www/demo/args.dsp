<%dsp:taglib name="demo"/>

<html>
<head>
  <title>DSP Example -- Tag arguments</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <p>This page demonstrates a tag call with arguments.

  <p><demo:show-keys arg1="100" arg2="foo"/>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
