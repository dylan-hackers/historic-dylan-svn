<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Platforms</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Platforms</h1></center>

<bt:show-messages/>

<a href="edit-platform.dsp?id=0&origin=list-platforms.dsp">Click here to create a new platform.</a>
<br>Click any platform name to edit that platform.

<p>

<dsp:table border="1" width="95%" align="center" generator="platform-generator">
  <dsp:hrow>
    <dsp:hcell>&nbsp;</dsp:hcell>
    <dsp:hcell>Name</dsp:hcell>
  </dsp:hrow>
  <dsp:no-rows>
    <dsp:cell colspan="4">There are no platforms to display.</dsp:cell>
  </dsp:no-rows>
  <dsp:row>
    <dsp:cell><dsp:row-number/></dsp:cell>
    <dsp:cell><a href="edit-platform.dsp?id=<bt:show-id/>"><bt:show-name/></a></dsp:cell>
  </dsp:row>
</dsp:table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
