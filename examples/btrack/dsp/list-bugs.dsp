<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Bug List</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Bug Report List</h1></center>

<bt:show-messages/>

<a href="edit-bug.dsp?id=new&type=bug-report&origin=/list-bugs.dsp">Click here to create a new bug report.</a>
<br>Click any bug report to edit that bug.

<p>

<dsp:table border="1" width="95%" align="center" generator="bug-report-generator">
  <dsp:hrow>
    <dsp:hcell>Bug#</dsp:hcell>
    <dsp:hcell>Priority</dsp:hcell>
    <dsp:hcell>Severity</dsp:hcell>
    <dsp:hcell>Reported By</dsp:hcell>
    <dsp:hcell>Product</dsp:hcell>
    <dsp:hcell>Module</dsp:hcell>
    <dsp:hcell>Synopsis</dsp:hcell>
  </dsp:hrow>
  <dsp:no-rows>
    <td colspan="6">There are no bugs to display.</td>
  </dsp:no-rows>
  <dsp:row>
    <dsp:cell width="2%" align="right"><bt:show-bug-number/></dsp:cell>
    <dsp:cell width="2%" align="center"><bt:show-priority/></dsp:cell>
    <dsp:cell width="2%" align="center"><bt:show-severity/></dsp:cell>
    <dsp:cell width="5%" align="center"><bt:show-reported-by/></dsp:cell>
    <dsp:cell width="5%" align="center"><bt:show-product/></dsp:cell>
    <dsp:cell width="5%" align="center"><bt:show-module/></dsp:cell>
    <dsp:cell width="79%"><a href="edit-bug.dsp?id=<bt:show-id/>&type=bug-report"><bt:show-synopsis/></a></dsp:cell>
  </dsp:row>
</dsp:table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
