<%dsp:taglib name="demo"/>

<html>
<head>
  <title>DSP Example -- Hello</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <p>This page demonstrates a simple tag call that displays &quot;hello world&quot;.

  <p><demo:hello/>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
