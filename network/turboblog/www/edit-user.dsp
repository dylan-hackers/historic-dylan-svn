<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="administration-meta-header.dsp"/>	
	<body>
	<%dsp:include url="administration-header.dsp"/>
	<%dsp:include url="administration-navigation.dsp"/>
		<div id="blog">
			<dsp:if test="edit-user?">
	    	<dsp:then>
					<form action="<turboblog:show-user-permanent-link />?edit" method="post">
				</dsp:then>
				<dsp:else>
					<form action="/users?add" method="post">
				</dsp:else>
			</dsp:if>	
				<fieldset id="general" class="splitted">
					<legend>General</legend>
					<ol>
						<li id="username">
							<label id="username-label" for="username-input">Username <em>*</em></label>
							<input id="username-input" type="text" name="username" value="<turboblog:show-user-username />"/>
							<dsp:when test="username-error?"><span class="error">An username is required.</span></dsp:when>
							<dsp:when test="user-exists-already-error?"><span class="error">This username is already in use.</span></dsp:when>
						</li>
						<li id="password">
							<label id="password-label" for="password-input">Password <em>*</em></label>
							<input id="password-input" type="password" name="password" />
							<dsp:when test="password-error?"><span class="error">A password is required.</span></dsp:when>
						</li>
					</ol>
				</fieldset>
				<fieldset id="contact" class="splitted">
					<legend>Contact</legend>
					<ol>
						<li id="email">
							<label id="email-label" for="email-input">E-Mail</label>
							<input id="email-input" type="text" name="email" value="<turboblog:show-user-email />"/>
						</li>
					</ol>
				</fieldset>
				<dsp:if test="edit-user?">
					<dsp:then>
						<input type="submit" value="Save"/>
					</dsp:then>
					<dsp:else>
						<input type="submit" value="Create"/>
					</dsp:else>
				</dsp:if>
			</form>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
