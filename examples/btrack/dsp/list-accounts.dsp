<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Accounts</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include uri="header.dsp"/>

<center><h1>Manage Accounts</h1></center>

<bt:show-errors-or-messages/>

<a href="record.dsp?id=new&type=account&origin=list-accounts">Click here to create a new account.</a>
<br>Click any account name to edit that account.

<p>

<dsp:table border="1" width="95%" align="center" generator="list-all-accounts">
  <dsp:hrow>
    <dsp:hcell nowrap>&nbsp;</dsp:hcell>
    <dsp:hcell nowrap>Name</dsp:hcell>
    <dsp:hcell nowrap>Email Address</dsp:hcell>
  </dsp:hrow>
  <dsp:row>
    <dsp:cell><dsp:row-number/></dsp:cell>
    <dsp:cell><a href="record.dsp?id=<bt:show-record-id/>&type=Account"><bt:show-account-name/></a></dsp:cell>
    <dsp:cell><bt:show-email-address/></dsp:cell>
  </dsp:row>
  <dsp:no-rows>
    <dsp:cell colspan="3">There are no user accounts.</dsp:cell>
  </dsp:no-rows>
</dsp:table>

<p>

<%dsp:include uri="footer.dsp" />

</body>
</html>
