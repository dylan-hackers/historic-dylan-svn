<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Operating Systems</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Operating Systems</h1></center>

<bt:show-messages/>

<a href="edit-operating-system.dsp?id=new&type=operating-system&origin=list-operating-systems.dsp">Click
 here to create a new operating system.</a>
<br>Click any operating system name to edit that operating system.

<p>

<dsp:table border="1" width="95%" align="center" generator="operating-system-generator">
  <dsp:hrow>
    <dsp:hcell>&nbsp;</dsp:hcell>
    <dsp:hcell>Name</dsp:hcell>
  </dsp:hrow>
  <dsp:no-rows>
    <dsp:cell colspan="2">There are no operating systems yet.</dsp:cell>
  </dsp:no-rows>
  <dsp:row>
    <dsp:cell><dsp:row-number/></dsp:cell>
    <dsp:cell><a href="edit-operating-system.dsp?id=<bt:show-id/>&type=operating-system"><bt:show-name/></a></dsp:cell>
  </dsp:row>
</dsp:table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
