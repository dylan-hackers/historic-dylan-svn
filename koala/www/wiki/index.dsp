<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Index</title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <h1>Index</h1>
    <wiki:show-index>
	<a href="/wiki/view.dsp?title=<wiki:show-title/>"><wiki:show-title/></a>
      <dsp:when test="admin?">
        <a href="/worker?title=<wiki:show-title/>&action=remove">remove</a>
        <form action="/worker" method="get">
          <input type="hidden" name="oldtitle" value="<wiki:show-title/>">
          <input type="hidden" name="action" value="rename">
          rename to <input type="text" name="title"><input type="submit" value="Save"/>
        </form>
      </dsp:when>
      <br/>
    </wiki:show-index>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
