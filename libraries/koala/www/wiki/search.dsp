<%dsp:taglib name="wiki"/>

<html>
<head>
  <title>Search Results</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <wiki:do-search-results>
    <br><a href="/wiki/view.dsp?title=<wiki:search-result-title/>&v=<wiki:search-result-version>"><wiki:search-result-title></a>
  </wiki:do-search-results>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
