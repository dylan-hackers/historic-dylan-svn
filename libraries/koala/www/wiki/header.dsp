<%dsp:taglib name="wiki"/>

<div id="header">
<!-- standard wiki header -->
<a href="/wiki/view.dsp?title=Home">Home</a>&nbsp;&nbsp;
<a href="/wiki/new.dsp">New Page</a>&nbsp;&nbsp;
<a href="/wiki/view.dsp?title=Markup">Wiki Markup</a>&nbsp;&nbsp;
<dsp:if test="editable?">
  <a href="/wiki/edit.dsp?title=<wiki:show-title v="true" for-url="true"/>">Edit This Page</a>&nbsp;&nbsp;
</dsp:if>
<form action="/wiki/search.dsp" method="post">
  <div id="search-form">
    <input type="text" name="search-terms" size="20"/>
    <input type="submit" value="search"/>
  </div>
</form>
<h1><wiki:show-title v="true" for-url="false"/></h1>
</div>
<!-- begin user-generated page content -->
