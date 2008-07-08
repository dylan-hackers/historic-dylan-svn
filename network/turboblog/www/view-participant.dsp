<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="administration-meta-header.dsp"/>	
	<body>
	<%dsp:include url="administration-header.dsp"/>
	<%dsp:include url="administration-navigation.dsp"/>
	<dsp:when test="blog?">
		<%dsp:include url="blog-navigation.dsp"/>
	</dsp:when>
		<div id="blog">
			<turboblog:with-participant-user>
			<fieldset>
				<legend>General</legend>
				<ol>
					<li id="user">
						<label for="user-field">User</label>
						<span id="user-field"><a href="<turboblog:show-user-permanent-link/>"><turboblog:show-user-username /></a></span>
					</li>
					<li id="role">
						<label for="role-field">Role</label>
						<span id="role-field"><turboblog:show-participant-role /></span>
					</li>
				</ol>
			</fieldset>
			</turboblog:with-participant-user>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
