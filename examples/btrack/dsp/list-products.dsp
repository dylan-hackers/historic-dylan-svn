<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Products</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<center><h1>Manage Products</h1></center>

<bt:show-messages/>

<a href="edit-product.dsp?id=new&type=product&origin=/list-products.dsp">Click here to create a new product.</a>
<br>Click any product name to edit that product.

<p>

<dsp:table border="1" width="95%" align="center" generator="product-generator">
  <dsp:hrow>
    <dsp:hcell>Name</dsp:hcell>
    <dsp:hcell>Owner</dsp:hcell>
    <dsp:hcell>Modules</dsp:hcell>
  </dsp:hrow>
  <dsp:no-rows>
    <dsp:cell colspan="3">There are no products yet.</dsp:cell>
  </dsp:no-rows>
  <dsp:row>
    <dsp:cell><a href="edit-product.dsp?id=<bt:show-id/>&type=product"><bt:show-name/></a></dsp:cell>
    <dsp:cell><bt:show-owner/></dsp:cell>
    <dsp:cell><bt:show-modules/></dsp:cell>
  </dsp:row>
</dsp:table>

<p>

<%dsp:include url="footer.dsp"/>

</body>
</html>
