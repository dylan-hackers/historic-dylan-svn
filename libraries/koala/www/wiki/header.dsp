<%dsp:taglib name="wiki"/>

<link  rel="stylesheet" href="/wiki/wiki.css"/>

<div id="header">
  <!-- standard wiki header -->
  <div id="logo"><p align="right"><a href="http://www.gwydiondylan.org/dylan-wiki.html" class="logo">Dylan Wiki</a></p></div>
  <form action="/wiki/search.dsp" method="post">
    <span class="search">
      <input type="text" name="search-terms" size="20"/>
      <input type="submit" value="search"/>
    </span>
  </form>
  <div id="navbar">
    <a href="/wiki/view.dsp?title=Home">Home</a>&nbsp;&nbsp;
    <a href="/wiki/new.dsp">New Page</a>&nbsp;&nbsp;
    <a href="/wiki/view.dsp?title=Markup">Wiki Markup</a>&nbsp;&nbsp;
    <dsp:if test="editable?">
      <a href="/wiki/edit.dsp?title=<wiki:show-title v="true" for-url="true"/>">Edit This Page</a>&nbsp;&nbsp;
    </dsp:if>
  </div>
</div>
<!-- begin user-generated page content -->
