<%dsp:taglib name="btrack" prefix="bt" %>

<html>
<head><title>Bug Tracker - Admin - Browsers</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include uri="header.dsp"/>

<center><h1>Manage Browsers</h1></center>

<bt:show-messages/>

<a href="record?id=new&type=Browser&origin=list-browsers">Click here to create a new browser.</a>
<br>Click any browser name to edit that browser.

<p>

<table border="1" width="95%" align="center">
  <bt:browser key="init_all" recordClass="sigue.btrack.Browser"/>
  <tr>
    <th nowrap>&nbsp;</th>
    <th nowrap>Name</th>
  </tr>
  <bt:iterate name="all_records" type="sigue.btrack.Browser">
    <bt:noRowsMessage>
      <tr><td colspan="4">There are no browsers to display.</td></tr>
    </bt:noRowsMessage>
    <bt:row>
      <tr>
        <td><bt:rowNumber/></td>
        <td><a href="record?id=<bt:browser key="id"/>&type=Browser"><bt:browser key="name"/></a></td>
      </tr>
    </bt:row>
  </bt:iterate>
</table>

<p>

<%dsp:include uri="footer.dsp"/>

</body>
</html>
