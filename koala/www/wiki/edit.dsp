<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title><wiki:show-page-title/></title>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <form action="/wiki/edit.dsp" method="post">
    Title: <input type="text" name="title" value="<wiki:show-page-title/>"/>
    <br/>
    <textarea name="page-content" cols="80" rows="20"><wiki:show-page-content format="raw"/></textarea>
    <br/>
    <input type="submit" value="Save"/>
  </form>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
