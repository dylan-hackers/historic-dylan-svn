<%dsp:taglib name="wiki"/>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Search Results</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>

  <dsp:show-form-notes/>

  <dsp:table border="0" align="left" cellspacing="2" generator="gen-search-results">
    <dsp:no-rows>
      <dsp:cell>No matches found.</dsp:cell>
    </dsp:no-rows>
    <dsp:row>
      <dsp:cell nowrap width="1%"><dsp:row-number/>.</dsp:cell>
      <dsp:cell nowrap width="10%"><a href="/wiki/view.dsp?title=<wiki:sr-title/>&v=<wiki:sr-version>"><wiki:sr-title> [<wiki:sr-version/>]</a></dsp:cell>
      <dsp:cell nowrap width="89%">
        <wiki:do-versions>
          <a href="/wiki/view.dsp?title=<wiki:sr-title/>&v=<wiki:sr-version>">[<wiki:sr-version/>]</a>
        </wiki:do-versions>
      </dsp:cell>
    </dsp:row>
    <dsp:row>
      <dsp:cell>&nbsp;</dsp:cell>
      <dsp:cell colspan="2"><wiki:sr-summary/></dsp:cell>
    </dsp:row>
  </dsp:table>

  <%dsp:include url="footer.dsp"/>

</body>
</html>
