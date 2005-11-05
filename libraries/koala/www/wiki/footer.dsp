<%dsp:taglib name="wiki"/>

<!-- standard wiki footer -->
<div id="footer">
  <a href="/wiki/recent.dsp">Recent&nbsp;Changes</a>&nbsp;&nbsp;
  <dsp:when test="editable?">
    <a href="/wiki/edit.dsp?title=<wiki:show-title v="true" for-url="true"/>">Edit This Page</a>&nbsp;&nbsp;
  </dsp:when>
  <dsp:if test="logged-in?">
    <dsp:then>
      Logged in as <wiki:username/>.
      <a href="/wiki/logout.dsp">Logout</a>
    </dsp:then>
    <dsp:else>
      <a href="/wiki/login.dsp">Login</a>
    </dsp:else>        
  </dsp:if>

  <wiki:show-revisions/>
  <p>
    <a href="http://validator.w3.org/check?uri=referer"><img src="http://www.w3.org/Icons/valid-xhtml10" alt="Valid XHTML 1.0!" height="31" width="88" /></a>
  </p>
</div>