<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Operating Systems</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Operating Systems</h1></center>

<bt:show-messages/>

<a href="record?id=new&type=OperatingSystem&origin=list-operating-systems">Click
 here to create a new operating system.</a>
<br>Click any operating system name to edit that operating system.

<p>

<table border="1" width="95%" align="center">
  <bt:operating_system key="init_all" recordClass="sigue.btrack.OperatingSystem"/>
  <tr>
    <th nowrap>&nbsp;</th>
    <th nowrap>Name</th>
  </tr>
  <bt:iterate name="all_records" type="sigue.btrack.OperatingSystem">
    <bt:noRowsMessage>
      <tr><td colspan="4">There are no operating systems to display.</td></tr>
    </bt:noRowsMessage>
    <bt:row>
      <tr>
        <td><bt:rowNumber/></td>
        <td><a href="record?id=<bt:operating_system key="id"/>&type=OperatingSystem"><bt:operating_system key="name"/></a></td>
      </tr>
    </bt:row>
  </bt:iterate>
</table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
