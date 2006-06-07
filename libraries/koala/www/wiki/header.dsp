<%dsp:taglib name="wiki"/>
<!-- standard wiki header -->
<div id="header">
  <div id="logo"><a href="http://wiki.opendylan.org/" class="logo">Dylan Wiki</a></div>
  <form action="/wiki/search.dsp" method="post">
    <div class="search">
      <input type="text" name="search-terms" size="20"/>
      <input type="submit" value="search"/>
    </div>
  </form>
  <div class="navbar">
    <a href="/wiki/view.dsp?title=Home">Home</a>&nbsp;&nbsp;
    <a href="/wiki/new.dsp">New Page</a>&nbsp;&nbsp;
    <a href="/wiki/view.dsp?title=Markup">Wiki Markup</a>&nbsp;&nbsp;
    <a href="/wiki/backlink.dsp?title=<wiki:show-title/>">Backlinks</a>&nbsp;&nbsp;
    <dsp:when test="editable?">
      <a href="/wiki/edit.dsp?title=<wiki:show-title v="true" for-url="true"/>">Edit This Page</a>&nbsp;&nbsp;
    </dsp:when>
  </div>
</div>
<!-- begin user-generated page content -->
