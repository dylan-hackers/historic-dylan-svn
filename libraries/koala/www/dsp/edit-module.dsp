<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Edit Module</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<dsp:show-form-notes/>

<center><h1>Edit Module</h1></center>

<form name="editModuleForm" method="post" action="edit-module.dsp">
  <dsp:show-hidden-fields/>
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Module name:</span>
        <br><input name="name"
                   type="text"
                   size="30"
                   maxlen="30"
                   value="<bt:show-name/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">User with primary responsibility:</span>
        <br><input name="owner"
                   type="text"
                   size="30"
                   maxlen="30"
                   value="<bt:show-owner/>">
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
