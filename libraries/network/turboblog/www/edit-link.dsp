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
			<dsp:if test="edit-link?">
	    	<dsp:then>
					<form action="<turboblog:show-link-permanent-link />?edit" method="post">
				</dsp:then>
				<dsp:else>
					<form action="<turboblog:show-blog-permanent-link />/links?add" method="post">
				</dsp:else>
			</dsp:if>	
				<fieldset>
					<legend>General</legend>
					<ol>
						<li id="title">
							<label id="title-label" for="title-input">Title <em>*</em></label>
							<input id="title-input" type="text" name="title" value="<turboblog:show-link-title />"/>
							<dsp:when test="title-error?"><span class="error">A title is required.</span></dsp:when>
							<dsp:when test="link-exists-already-error?"><span class="error">This title is already in use.</span></dsp:when>
						</li>
						<li id="url">
              <label id="url-label" for="url-input">Address <em>*</em></label>
              <input id="url-input" type="text" name="url" value="<turboblog:show-link-url />"/>
              <dsp:when test="url-error?"><span class="error">An url is required.</span></dsp:when>
            </li>
 						<li id="description">
              <label id="description-label" for="description-text">Description:</label>
              <textarea id="description-text" name="description" cols="70" rows="20"><turboblog:show-link-description /></textarea>
            </li>
            <li id="tags">
              <label id="tags-label" for="tags-input">Tags:</label>
              <input id="tags-input" type="text" name="tags" value="<turboblog:list-link-tags><turboblog:show-tag /> </turboblog:list-link-tags>"/>
					</li>
				</ol>
				</fieldset>
				<dsp:if test="edit-link?">
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
