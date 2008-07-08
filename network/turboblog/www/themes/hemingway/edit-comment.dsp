<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="meta-header.dsp"/>	
	<body>
	<%dsp:include url="header.dsp"/>
  	<div class="entry">
      <form action="?<dsp:if test="edit-comment?"><dsp:then>edit</dsp:then><dsp:else>add</dsp:else></dsp:if>" method="post">
				<fieldset>
					<ol>
						<li id="name">
							<label id="name-label" for="name-input">Name:</label>
							<dsp:if test="name-error?">
								<dsp:then>
									<input id="name-input" type="text" name="name" value="" />
									<span class="error">You have to enter a name!</span>
								</dsp:then>
								<dsp:else>
									<input id="name-input" type="text" name="name" value="<turboblog:show-comment-name />" />
								</dsp:else>
							</dsp:if>
						</li>
						<li id="website">
							<label id="website-label" for="website-input">Website:</label>
							<input id="website-input" type="text" name="website" value="<turboblog:show-comment-website>"/>
						</li>
						<li id="body">
							<label id="body-label" for="body-text">Body:</label>
							<dsp:if test="body-error?">
								<dsp:then>
									<textarea id="body-text" name="body" cols="70" rows="20"></textarea>
									<span class="error">You have to enter a body!</span>
								</dsp:then>
								<dsp:else>
									<textarea id="body-text" name="body" cols="70" rows="20"><turboblog:show-comment-body /></textarea>
							 </dsp:else>
							</dsp:if>
						</li>
					</ol>
				</fieldset>
				<input type="submit" value="<dsp:if test="edit-comment?"><dsp:then>Save</dsp:then><dsp:else>Comment</dsp:else></dsp:if>" />
			</form>
    </div>
		<%dsp:include url="footer.dsp"/>
	</body>
</html>
