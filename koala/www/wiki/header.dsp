<%dsp:taglib name="wiki"/>


<div id="header">
  <!-- standard wiki header -->
  <div id="logo"><p><a href="http://www.gwydiondylan.org/dylan-wiki.html" class="logo">Dylan Wiki</a></p></div>
  <form action="/wiki/search.dsp" method="post">
    <div class="search">
      <input type="text" name="search-terms" size="20"/>
      <input type="submit" value="search"/>
    </div>
  </form>
  <div id="navbar">
    <a href="/wiki/view.dsp?title=Home">Home</a>&nbsp;&nbsp;
    <a href="/wiki/new.dsp">New Page</a>&nbsp;&nbsp;
    <a href="/wiki/view.dsp?title=Markup">Wiki Markup</a>&nbsp;&nbsp;
    <dsp:when test="editable?">
      <a href="/wiki/edit.dsp?title=<wiki:show-title v="true" for-url="true"/>">Edit This Page</a>&nbsp;&nbsp;
    </dsp:when>
    <dsp:if test="logged-in?">
      <dsp:then>
        Logged in as <wiki:username/>.
      </dsp:then>
      <dsp:else>
        <form action="/wiki/view.dsp" method="post">
          <div class="login">
            <input type="text" name="username" size="20"/>
            <input type="submit" value="login"/>
          </div>
        </form>
      </dsp:else>        
    </dsp:if>
  </div>
</div>
<!-- begin user-generated page content -->
