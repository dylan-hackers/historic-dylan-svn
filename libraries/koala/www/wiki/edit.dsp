<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: <wiki:show-title/></title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <div id="form-notes">
  <dsp:show-form-notes/>
  </div>

  <dsp:if test="logged-in?">
    <dsp:then>
      <form action="/wiki/edit.dsp" method="post">
        <div id="edit">
          Title: <input type="text" name="title" value="<wiki:show-title/>"/>
          <br/>
          <textarea name="page-content" cols="80" rows="20"><wiki:show-content format="raw"/></textarea>
          <br/>
          <input type="submit" value="Save"/>
        </div>
      </form>
    </dsp:then>
    <dsp:else>
      Error: you're not allowed to edit <wiki:show-title/>.
    </dsp:else>
  </dsp:if>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
