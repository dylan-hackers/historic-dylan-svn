<%dsp:taglib name="msg-taglib" prefix="msg" />

<html>
<head><title>Web Object Example - Edit Message</title></head>

<body bgcolor="#FFFFFF">

<center><h1>Edit Message</h1></center>

<form name="editMessageForm" method="post" action="edit-message.dsp">
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Enter message:</span>
        <br><textarea name="message" rows="5" cols="40">
              <msg:show-message/>
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
