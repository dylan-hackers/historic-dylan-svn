<%dsp:taglib name="btrack" prefix="bt" />
<jsp:useBean id="record" class="sigue.btrack.Platform" scope="session"/>

<html>
<head><title>Bug Tracker - Admin - Edit Platform</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<bt:show-messages/>

<center><h1>Edit Platform</h1></center>

<form name="editPlatformForm" method="post" action="edit-platform.dsp">
  <input type="hidden" name="type" value="Platform">
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Platform name:</span>
        <br><input name="name" type="text" size="30" maxlen="30"
                   value="<bt:show-name/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <input name="Submit" type="Submit" value="Submit">
      </td>
    </tr>
  </table>

</form>

<%dsp:include url="footer.dsp"/>

</body>
</html>
