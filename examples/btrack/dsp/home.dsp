<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker Home</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center> <h1>Welcome to Bug Tracker</h1> </center>

<p> <bt:show-messages/>

<center>
  <bt:show-image name="bug.gif" />
  <p>
  <p>
  <p>
  <table border="0">
    <tr>
      <td>
        <font size="+2">
          <ol>
            <li><a href="edit-bug.dsp?id=new&type=bug-report&origin=home.dsp">Enter a new bug report</a></li>
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

<%dsp:include url="footer.dsp"/>

</body>
</html>
