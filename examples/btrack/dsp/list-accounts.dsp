<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Accounts</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Accounts</h1></center>

<bt:show-messages/>

<a href="edit-account.dsp?id=0&origin=list-accounts.dsp">Click here to create a new account.</a>
<br>Click any account name to edit that account.

<p>

<dsp:table border="1" width="95%" align="center" generator="list-accounts">
  <dsp:hrow>
    <dsp:hcell>&nbsp;</dsp:hcell>
    <dsp:hcell>Name</dsp:hcell>
    <dsp:hcell>Email Address</dsp:hcell>
  </dsp:hrow>
  <dsp:row>
    <dsp:cell><dsp:row-number/></dsp:cell>
    <dsp:cell><a href="edit-account.dsp?id=<bt:show-id/>"><bt:show-name/></a></dsp:cell>
    <dsp:cell><bt:show-email-address/></dsp:cell>
  </dsp:row>
  <dsp:no-rows>
    <dsp:cell colspan="3">There are no user accounts.</dsp:cell>
  </dsp:no-rows>
</dsp:table>

<p>

<%dsp:include url="footer.dsp" />

</body>
</html>
