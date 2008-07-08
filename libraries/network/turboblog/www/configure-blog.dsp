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
			<dsp:if test="configure-blog?">
		  	<dsp:then>
					<form action="<turboblog:show-blog-permanent-link />?configure" method="post">
				</dsp:then>
				<dsp:else>
					<form action="/blogs?add" method="post">
				</dsp:else>
			</dsp:if>
				<fieldset id="general" class="splitted">
					<legend>General</legend>
					<ol id="title">
						<li>
							<label id="title-label" for="title-input">Title <em>*</em></label>
							<input id="title-input" type="text" name="title" value="<turboblog:show-blog-title />" />
							<dsp:when test="title-error?"><span class="error">An title is required.</span></dsp:when>
						  <dsp:when test="blog-exists-already-error?"><span class="error">This title is already in use.</span></dsp:when>
						</li>
						<li id="subtitle">
							<label id="subtitle-label" for="subtitle-input">Subtitle</label>
							<input id="subtitle-input" type="text" name="subtitle" value="<turboblog:show-blog-subtitle />" />
						</li>
						<li id="description">
							<label id="description-label" for="description-text">Description</label>
							<textarea id="description-text" name="description" cols="40" rows="10"><turboblog:show-blog-description escape="true"/></textarea>
						</li>
					</ol>
				</fieldset>
				<fieldset id="style" class="splitted">
          <label>Style</label>
          <ol>
             <li id="theme">
						 	<label id="theme-label" for="theme-select">Theme</label>
							<select id="theme-select" name="theme">
								<turboblog:list-themes>
									<option<dsp:when test="blog-theme?"> selected="selected"</dsp:when>><turboblog:show-theme/></option>
								</turboblog:list-themes>
							</select>
						</li>
						<li id="show-entry-authors">
							<input id="show-entry-authors-checkbox" type="checkbox" name="show-entry-authors" <dsp:when test="blog-show-entry-authors?">checked="checked"</dsp:when>/>
							<label id="show-entry-authors-label" for="show-entry-authors-checkbox">Show authors for entry</label>
						</li>
          </ol>
				</fieldset>
				<dsp:if test="configure-blog?">
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
