<%dsp:taglib name="wiki"/>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Search Results</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <wiki:do-search-results>
    <br/><a href="/wiki/view.dsp?title=<wiki:search-result-title/>&v=<wiki:search-result-version>"><wiki:search-result-title></a>
  </wiki:do-search-results>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
