<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Browsers</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Browsers</h1></center>

<bt:show-messages/>

<a href="edit-browser.dsp?id=0&origin=list-browsers.dsp">Click here to create a new browser.</a>
<br>Click any browser name to edit that browser.

<p>

<dsp:table border="1" width="95%" align="center" generator="browser-generator">
  <dsp:hrow>
    <dsp:hcell>&nbsp;</dsp:hcell>
    <dsp:hcell>Name</dsp:hcell>
  </dsp:hrow>
  <dsp:no-rows>
    <dsp:cell colspan="2">There are no browsers to display.</dsp:cell>
  </dsp:no-rows>
  <dsp:row>
    <dsp:cell><bt:row-number/></dsp:cell>
    <dsp:cell><a href="edit-browser.dsp?id=<bt:show-id/>"><bt:show-name/></a></dsp:cell>
  </bt:row>
</dsp:table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
