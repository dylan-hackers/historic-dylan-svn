<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="administration-meta-header.dsp"/>	
	<body>
		<%dsp:include url="administration-header.dsp"/>
		<%dsp:include url="administration-navigation.dsp"/>
		<div id="blog" class="group">
			<fieldset>
				<legend>General</legend>
				<ol>
					<li id="username">
						<label for="username-field">Username</label>	
						<span id="username-field"><turboblog:show-user-username /></span>
					</li>
				</ol>
			</fieldset>
      <fieldset>
				<legend>Contact</legend>
				<ol>
					<li id="email">
						<label for="email-field">E-Mail</label>
						<span id="email-field"><turboblog:show-user-email /></span>
					</li>
				</ol>
			</fieldset>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
