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
      <dsp:no-rows>
        <!-- no-rows is only displayed if the table contains no rows. -->
        <dsp:cell>There are no rows to display.</dsp:cell>
      </dsp:no-rows>
      <dsp:hrow>
        <!-- hrow is always displayed exactly once, even if the table contains no rows. -->
        <dsp:hcell>Row</dsp:hcell>
        <dsp:hcell>English</dsp:hcell>
        <dsp:hcell>Spanish</dsp:hcell>
      </dsp:hrow>
      <dsp:row>
        <!-- rows are only displayed once for each item of table data. -->
        <dsp:cell><dsp:row-number/></dsp:cell>
        <dsp:cell><demo:column1-data/></dsp:cell>
        <dsp:cell><demo:column2-data/></dsp:cell>
      </dsp:row>
    </dsp:table>

    <p>
    The same table, with a row generator that returns no rows:<br>
    <dsp:table border="1" align="center" cellspacing="2" generator="no-rows-generator">
      <dsp:hrow>
        <!-- hrow is always displayed exactly once, even if the table contains no rows. -->
        <dsp:hcell>Row</dsp:hcell>
        <dsp:hcell>English</dsp:hcell>
        <dsp:hcell>Spanish</dsp:hcell>
      </dsp:hrow>
      <dsp:row>
        <!-- rows are only displayed once for each item of table data. -->
        <dsp:cell><dsp:row-number/></dsp:cell>
        <dsp:cell><demo:column1-data/></dsp:cell>
        <dsp:cell><demo:column2-data/></dsp:cell>
      </dsp:row>
      <dsp:no-rows>
        <!-- no-rows is only displayed if the table contains no rows. -->
        <dsp:cell colspan="3">There are no rows to display.</dsp:cell>
      </dsp:no-rows>
    </dsp:table>
  </form>

  <%dsp:include url="body-wrapper-end.dsp"/>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
