<%dsp:taglib name="btrack" prefix="bt" />

<html>
<head><title>Bug Tracker - Admin - Edit Product</title></head>

<body bgcolor="#FFFFFF">

<%dsp:include url="header.dsp"/>

<dsp:show-form-notes/>

<center><h1>Edit Product</h1></center>

<form name="editProductForm" method="post" action="edit-product.dsp">
  <input type="hidden" name="type" value="Product">
  <table border="0">
    <tr>
      <td width="5%">&nbsp;</td>
      <td nowrap width="95%" colspan="3">
        <span class="fieldTitle">Product name:</span>
        <br><input name="name" type="text" size="30" maxlen="30"
                   value="<bt:show-name/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <span class="fieldTitle">User with primary responsibility:</span>
        <br><input name="owner" type="text" size="30" maxlen="30"
                   value="<bt:show-owner/>">
      </td>
    </tr>
    <tr>
      <td width="5%">&nbsp;</td>
      <td width="95%" colspan="3">
        <input name="Submit" type="Submit" value="Submit">
      </td>
    </tr>
  </table>

</form>

<p>

<a href="edit-module.dsp?id=new&type=module&origin=/edit-product.dsp&product_id=<dsp:show-id/>">Click here to create a new module for this product.</a>
<br>Click any module name to edit that module.

<p>

  <dsp:table border="1" width="95%" align="center" generator="gen-product-modules">
    <dsp:hrow>
      <dsp:hcell>Name</dsp:hcell>
    </dsp:hrow>
    <dsp:row>
      <dsp:cell><a href="edit-module.dsp?id=<dsp:show-id key='row'/>&type=module&product-id=<dsp:show-id/>"><bt:show-name key='row'/></a></dsp:cell>
    </dsp:row>
    <dsp:no-rows>
      <dsp:cell>This product has no modules yet.</dsp:cell>
    </dsp:no-rows>
  </dsp:table>

<p>

<a href="edit-version.dsp?id=new&type=version&origin=/edit-product.dsp&product_id=<dsp:show-id/>">Click here to create a new version for this product.</a>
<br>Click any version name to edit that version.

<p>

  <dsp:table border="1" width="95%" align="center" generator="gen-product-versions">
    <dsp:hrow>
      <dsp:hcell>Name</dsp:hcell>
    </dsp:hrow>
    <dsp:row>
      <dsp:cell><a href="edit-version.dsp?id=<dsp:show-id key='row'/>&type=version&product-id=<dsp:show-id/>"><bt:show-name key='row'/></a></dsp:cell>
    </dsp:row>
    <dsp:no-rows>
      <dsp:cell>This product has no versions yet.</dsp:cell>
    </dsp:no-rows>
  </dsp:table>

<%dsp:include url="footer.dsp"/>

</body>
</html>
