<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: <wiki:show-title/></title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <dsp:if test="login?">
      <dsp:then>
        Error logging out user!
      </dsp:then>
      <dsp:else>
        User logged out! <a href="<dsp:show-referer/>">back</a>
      </dsp:else>
    </dsp:if>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
