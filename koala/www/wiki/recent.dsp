<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Recent Changes</title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">

    <dsp:show-form-notes/>
    <h3>Recent Changes</h3>
    <ul>
    <wiki:gen-recent-changes count="50">
      <li><wiki:show-change-timestamp/> <a href="/wiki/view.dsp?title=<wiki:show-change-title/>&v=<wiki:show-change-version/>"><wiki:show-change-title/></a> version <wiki:show-change-version/> <a href="/wiki/diff.dsp?title=<wiki:show-change-title/>&version=<wiki:show-change-version/>">diff</a> by <wiki:show-change-author/> Comment <wiki:show-change-comment/></li>
    </wiki:gen-recent-changes>
    </ul>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
