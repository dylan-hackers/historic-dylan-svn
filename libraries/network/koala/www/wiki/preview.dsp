<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Preview of <wiki:show-title/></title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <h1>Preview for "<wiki:show-title v="false" for-url="false"/>"</h1>
  <form action="/wiki/edit.dsp" method="post">
    <div id="edit">
       <input type="hidden" name="title" value="<wiki:show-title/>"/>
       <wiki:show-content format="html"/><br/><hr/>
       <textarea name="page-content" cols="80" rows="20"><wiki:show-content format="raw"/></textarea><br/>
       Comment: <input type="text" name="comment" value="<wiki:show-comment/>"/><br/>
       <input type="submit" name="preview" value="Preview"/>
       <input type="submit" name="save" value="Save"/>
      </div>
    </form>

  <%dsp:include url="footer.dsp"/>
</body>
</html>
