<%dsp:taglib name="wiki"/>

<html>
<head>
  <title><wiki:show-page-title/></title>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <form action="/wiki/edit.dsp" method="post">
    Title: <input type="text" name="title" value="<wiki:show-page-title/>" width="30">
    <br>
    <textarea name="page-content" cols=80 rows=20><wiki:show-page-content format="raw"/></textarea>
    <br>
    <input type="submit" value="Save">
  </form>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
