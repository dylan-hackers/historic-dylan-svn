<html>
<head>
  <title>Please Login</title>
</head>

<body>
  <form action="http://localhost:7020/test.dsp" method="post" enctype="application/x-www-form-urlencoded">
    <h3 align="center">Please Login</h3>
    <table border="0" align="center" cellspacing="2">
      <tr>
        <td nowrap align="right">User name:</td>
        <td nowrap><input name="username" value="<dsp:current-username/>" type="text"></td>
      </tr>
      <tr>
        <td nowrap align="right">Password:</td>
        <td nowrap><input name="password" value="" type="password"></td>
      </tr>
      <tr>
        <td nowrap align="right" colspan="2"><input name="submit" value="Login" type="submit"></td>
      </tr>
    </table>
  </form>

</body>
</html>
