<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Login</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<dsp:show-form-notes/>

<h1>Please Login:</h1>

<form name="loginForm" method="post" action="login.dsp">
  <dsp:show-hidden-fields/>
  <table border="0">
    <tr>
      <td align="right">Username:</td>
      <td><input type="text" name="username" value="<dsp:show-query-value name='username'/>" size="20"></td>
    </tr>
    <tr>
      <td align="right">Password:</td>
      <td><input type="password" name="password" value="<dsp:show-query-value name='password'/>" size="20"></td>
    </tr>
    <tr>
      <td colspan="2" align="right">
        <input type="submit" name="submit" value="Login">
      </td>
    </tr>
  </table>
</form>

<%dsp:include url="footer.dsp"/>

</body>
</html>
