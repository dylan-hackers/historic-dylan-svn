<%dsp:taglib name="example" prefix="demo"/>

<html>
<head>
  <title>DSP Example -- Table Generation</title>
</head>

<body>

  <%dsp:include url="header.dsp"/>
  <%dsp:include url="body-wrapper-start.dsp"/>

  <demo:show-errors/>

  <form action="welcome.dsp" method="post" enctype="application/x-www-form-urlencoded">
    <h2>Table Generation</h2>

    <p>This page demonstrates ...

    <p>
    <table border="1" align="center" cellspacing="2">
      <%dsp:if test="table-has-rows?">
        <demo:demo-table row-generator="demo-row-generator">
          <tr>
            <td><demo:column1-data/></td>
            <td><demo:column2-data/></td>
          </tr>
        </demo:demo-table>
      <%dsp:else>
        <tr><td>There are now rows to display.</td></tr>
      </%dsp:if>
    </table>
  </form>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
