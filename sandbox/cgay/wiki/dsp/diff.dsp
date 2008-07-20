<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Diff <wiki:show-title/></title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <h1>Diff <wiki:show-title/> Versions <wiki:version/> - <wiki:otherversion/></h1>
    <wiki:show-diff/>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
