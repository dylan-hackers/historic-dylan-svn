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

    <p>This page demonstrates the use of the dsp:table tag to generate HTML tables from a
    set of data, using dsp:table and %dsp:if.

    <p>
    A table with a bunch of rows:<br>
    <table border="1" align="center" cellspacing="2">
      <%dsp:if test="table-has-rows1?">
        <dsp:table row-generator="animal-generator">
          <tr bgcolor="<demo:row-bgcolor/>">
            <td><dsp:table-row-number/></td>
            <td><demo:column1-data/></td>
            <td><demo:column2-data/></td>
          </tr>
        </dsp:table>
      <%dsp:else>
        <tr><td>There are no rows to display.</td></tr>
      </%dsp:if>
    </table>

    <p>
    A table with no rows:<br>
    <table border="1" align="center" cellspacing="2">
      <%dsp:if test="table-has-rows2?">
        <dsp:table row-generator="no-rows-generator">
          <!-- It doesn't really matter what goes here, since there are no rows
               for this table and the body of this tag will never be displayed. -->
          <tr>
            <td><dsp:table-row-number/></td>
            <td><demo:column1-data/></td>
            <td><demo:column2-data/></td>
          </tr>
        </dsp:table>
      <%dsp:else>
        <tr><td>There are no rows to display.</td></tr>
      </%dsp:if>
    </table>
  </form>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
