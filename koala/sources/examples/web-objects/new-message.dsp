<%dsp:taglib name="msg-taglib" prefix="msg" />

<html>
<head><title>Web Object Example - New Message</title></head>

<body bgcolor="#FFFFFF">

<center><h1>New Message</h1></center>

<form name="newMessageForm" method="post" action="edit-message.dsp">
  <input type="hidden" name="new-record" value="true"/>
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Enter new message:</span>
        <br><textarea name="message" rows="5" cols="40">
            </textarea>
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

</body>
</html>
