<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker Home</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include uri="header.dsp"/>

<center> <h1>Welcome to Bug Tracker</h1> </center>

<p> <bt:show-messages/>

<center>
  <img src="<bt:image name="bug.gif"/>">
  <p>
  <p>
  <p>
  <table border="0">
    <tr>
      <td>
        <font size="+2">
          <ol>
            <li><a href="record?id=new&type=Bug&origin=home">Enter a new bug report</a></li>
            <li><a href="nyi.dsp">Search bug reports</a></li>
            <li><a href="list-bugs.dsp">List recent bug reports</a></li>
            <li><a href="admin.dsp">Administration</a></li>
            <li><a href="nyi.dsp">Reports</a></li>
          </ol>
        </font>
      </td>
    </tr>
  </table>
</center>

<%dsp:include uri="footer.dsp"/>

</body>
</html>
