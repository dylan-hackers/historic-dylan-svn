<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Versions of Page <wiki:show-title/></title>
  <link rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">

    <dsp:show-form-notes/>
    <h3>Version History of <wiki:show-title/></h3>
    <ul>
    <wiki:show-versions>
      <li><wiki:show-change-timestamp/> version <wiki:show-change-version/> <a href="/wiki/diff.dsp?title=<wiki:show-change-title/>&version=<wiki:show-change-version/>">diff to previous</a> by <wiki:show-change-author/> Comment <wiki:show-change-comment/></li>
    </wiki:show-versions>
    </ul>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
