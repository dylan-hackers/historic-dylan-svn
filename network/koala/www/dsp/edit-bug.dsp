<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head>
  <title>Bug Tracker -- Edit Bug Report</title>
</head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Edit Bug Report</h1></center>

<dsp:show-form-notes/>

<form name="newBugForm" method="post" action="edit-bug.dsp">
  <dsp:show-hidden-fields/>
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">Summary:</span>
        <br><input name="synopsis"
                   type="text"
                   size="80"
                   maxlen="100"
                   value="<bt:show-synopsis/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">Description:</span>
        <br><textarea name="description" wrap="virtual" rows="15" cols="80"><bt:show-description/></textarea>
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <table border="0">
          <tr>
            <td nowrap><span class="fieldTitle">Priority:</span></td>
            <td nowrap><select name="priority" size="1"><bt:show-priority-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Severity:</span></td>
            <td nowrap><select name="severity" size="1"><bt:show-severity-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Version:</span></td>
            <td nowrap><select name="version" size="1"><bt:show-version-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Operating System:</span></td>
            <td nowrap><select name="operating_system" size="1"><bt:show-operating-system-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Hardware Platform:</span></td>
            <td nowrap><select name="platform" size="1"><bt:show-platform-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Browser:</span></td>
            <td nowrap><select name="browser" size="1"><bt:show-browser-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Product:</span></td>
            <td nowrap><select name="product" size="1"><bt:show-product-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Module:</span></td>
            <td nowrap><select name="module" size="1"><bt:show-module-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Assigned Developer:</span></td>
            <td nowrap><select name="dev_assigned" size="1"><bt:show-dev-assigned-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Assigned QA:</span></td>
            <td nowrap><select name="qa_assigned" size="1"><bt:show-qa-assigned-options/></select></td>
          </tr>
          <tr>
            <td nowrap><span class="fieldTitle">Status:</span></td>
            <td nowrap><select name="status" size="1"><bt:show-status-options/></select></td>
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

<%dsp:include url="footer.dsp"/>

</body>
</html>
