<%dsp:taglib name="btrack" prefix="bt" />
<jsp:useBean id="record" class="sigue.btrack.OperatingSystem" scope="session"/>

<html>
<head><title>Bug Tracker - Admin - Edit Operating System</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<bt:show-messages/>

<center><h1>Edit Operating System</h1></center>

<form name="editOperatingSystemForm" method="post" action="record?action=save">
  <input type="hidden" name="type" value="OperatingSystem">
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Operating system name:</span>
        <br><input name="name" type="text" size="30" maxlen="30"
                   value="<jsp:getProperty name="record" property="name"/>">
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
