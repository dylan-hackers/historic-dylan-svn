<%dsp:taglib name="wiki"/>

<html>
<head>
  <title><wiki:show-page-title/></title>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <wiki:show-page-content format="html"/>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
