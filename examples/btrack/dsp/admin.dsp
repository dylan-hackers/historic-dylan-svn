<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin Menu</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<bt:show-messages/>

<center>
  <h1>Administration Menu</h1>
  <p>
  <bt:show-image name="bug.gif" />
  <p>
  <p>
  <p>
  <table border="0">
    <tr>
      <td>
        <font size="+2">
          <ol>
            <li><a href="list-accounts.dsp">Manage Accounts</a></li>
            <li><a href="list-products.dsp">Manage Products</a></li>
            <li><a href="list-browsers.dsp">Manage Browsers</a></li>
            <li><a href="list-operating-systems.dsp">Manage Operating Systems</a></li>
            <li><a href="list-platforms.dsp">Manage Hardware Platforms</a></li>
          </ol>
        </font>
      </td>
    </tr>
  </table>
</center>

<%dsp:include url="footer.dsp"/>

</body>
</html>
