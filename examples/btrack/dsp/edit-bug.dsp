<%dsp:taglib name="btrack" prefix="bt" %>
<jsp:useBean id="record" class="sigue.btrack.Bug" scope="session"/>

<html>
<head>
  <title>Bug Tracker -- Edit Bug Report</title>
  <bt:stylesheet/>
</head>

<body bgcolor="#FFFFFF">

<%dsp:include uri="header.dsp"/>

<center><h1>Edit Bug Report</h1></center>

<bt:show-messages/>

<form name="newBugForm" method="post" action="record?action=save">
  <input type="hidden" name="type" value="Bug">
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">Summary:</span>
        <br><input name="synopsis" type="text" size="80" maxlen="100"
                   value="<jsp:getProperty name="record" property="synopsis"/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">Description:</span>
        <br><textarea name="description" wrap="virtual" rows="15" cols="80"><jsp:getProperty name="record" property="description"/></textarea>
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <table border="0">
          <tr>
            <td nowrap><span class="fieldTitle">Priority:</span></td>
            <td nowrap><select name="priority" size="1"><bt:bug key="priority"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Severity:</span></td>
            <td nowrap><select name="severity" size="1"><bt:bug key="severity"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Version:</span></td>
            <td nowrap><select name="version" size="1"><bt:bug key="version"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Operating System:</span></td>
            <td nowrap><select name="operating_system" size="1"><bt:bug key="operating_system"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Hardware Platform:</span></td>
            <td nowrap><select name="platform" size="1"><bt:bug key="platform"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Browser:</span></td>
            <td nowrap><select name="browser" size="1"><bt:bug key="browser"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Product:</span></td>
            <td nowrap><select name="product" size="1"><bt:bug key="product"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Module:</span></td>
            <td nowrap><select name="module" size="1"><bt:bug key="module"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Assigned Developer:</span></td>
            <td nowrap><select name="dev_assigned" size="1"><bt:bug key="dev_assigned"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Assigned QA:</span></td>
            <td nowrap><select name="qa_assigned" size="1"><bt:bug key="qa_assigned"/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Status:</span></td>
            <td nowrap><select name="status" size="1"><bt:bug key="status"/></select></td>
          </tr>
        </table>
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

<%dsp:include uri="footer.dsp"/>

</body>
</html>
