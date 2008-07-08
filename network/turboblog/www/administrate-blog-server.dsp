<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="administration-meta-header.dsp"/>	
	<body>
	<%dsp:include url="administration-header.dsp"/>
	<%dsp:include url="administration-navigation.dsp"/>
		<div class="body">	
			<form action="/?administrate" method="post">
				<fieldset>
					<legend>General</legend>
					<ol>
						<li id="title">
							<label id="title-label" for="title-input">Title <em>*</em></label>
							<input id="title-input" type="text" name="title" value="<turboblog:show-blog-server-title />"/>
							<dsp:when test="title-error?"><span class="error">An title is required.</span></dsp:when>
						</li>
					</ol>
				</fieldset>	
				<input type="submit" value="Save"/>
			</form>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
