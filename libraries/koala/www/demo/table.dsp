<%dsp:taglib name="demo"/>

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
    set of data, using dsp:table and dsp:if.

    <p>
    A table with a bunch of rows:<br>
    <dsp:table border="1" align="center" cellspacing="2" generator="animal-generator">
      <!-- hrows are always displayed, even if the table contains no rows. -->
      <dsp:hrow>
        <dsp:hcell>#</dsp:hcell>
        <dsp:hcell>English</dsp:hcell>
        <dsp:hcell>Spanish</dsp:hcell>
      </dsp:hrow>
      <!-- no-rows-message is only displayed if the table contains no rows. -->
      <dsp:no-rows-message>
        <dsp:cell>There are no rows to display.</dsp:cell>
      </dsp:no-rows-message>
      <!-- rows are only displayed if the table contains at least one row. -->
      <dsp:row>
        <td><dsp:row-number/></td>
        <td><demo:column1-data/></td>
        <td><demo:column2-data/></td>
      </dsp:row>
    </dsp:table>

    <p>
    A table with no rows:<br>
    <table border="1" align="center" cellspacing="2">
      <dsp:if test="table-has-rows2?">
        <dsp:then>
          <dsp:table row-generator="no-rows-generator">
            <!-- It doesn't really matter what goes here, since there are no rows
                 for this table and this body will never be displayed. -->
            <tr>
              <td><dsp:table-row-number/></td>
              <td><demo:column1-data/></td>
              <td><demo:column2-data/></td>
            </tr>
          </dsp:table>
        </dsp:then>
        <dsp:else>
          <tr><td>There are no rows to display.</td></tr>
        </dsp:else>
      </dsp:if>
    </table>
  </form>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
