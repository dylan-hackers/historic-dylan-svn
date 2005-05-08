<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: <wiki:show-title/></title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <dsp:if test="logged-in?">
      <dsp:then>
        Logged in as <wiki:username/>. <a href="/wiki/logout.dsp">Logout</a>
      </dsp:then>
      <dsp:else>
        <dsp:show-form-notes/>
        <form action="/wiki/login.dsp" method="post">
          <div class="login">
            Username: <input type="text" name="username" size="20"/> <br/>
            Password: <input type="password" name="password" size="20"/> <br/>
            <input type="submit" value="login"/>
          </div>
        </form>
      </dsp:else>        
    </dsp:if>
  </div>
  <%dsp:include url="footer.dsp"/>
</body>
</html>
