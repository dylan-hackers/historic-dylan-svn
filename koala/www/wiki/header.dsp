<%dsp:taglib name="wiki"/>

<!-- standard wiki header -->
<a href="/wiki/view.dsp?title=Home">Home</a>
<a href="/wiki/new.dsp">Create New Page</a>
<a href="/wiki/view.dsp?title=Markup">Wiki Markup</a>
<a href="/wiki/edit.dsp?title=<wiki:show-page-title/>">Edit This Page</a>
<form action="/wiki/search.dsp" method="post">
  <input type="text" name="search-terms" size="20"/>
  <input type="submit" value="search"/>
</form>
<h1><wiki:show-page-title/></h1>
<!-- begin user-generated page content -->
