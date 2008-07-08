<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <%dsp:include url="administration-meta-header.dsp"/>
	<body>
    <%dsp:include url="administration-header.dsp"/>
    <%dsp:include url="administration-navigation.dsp"/>
	  <%dsp:include url="blog-navigation.dsp"/>
		<div id="blog">
			<form action="?remove" method="post">
      	<p></p>
      	<input type="submit" value="Remove '<turboblog:show-link-title/>'"/>
			</form>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
