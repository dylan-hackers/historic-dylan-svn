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
			<fieldset>
				<legend>General</legend>
				<ol>
					<li id="title">
						<label for="title-field">Title</label>
						<span id="title-field"><turboblog:show-link-title /></span>
					</li>
					<li id="url">
						<label for="url-field">Address</label>
						<span id="url-field"><turboblog:show-link-url /></span>
					</li>
					<li id="description">
						<label for="description-field">Description:</label>
						<p id="description-field"><turboblog:show-link-description /></p>
					</li>
					<li id="tags">
						<label for="tags-field">Tags:</label>
						<span id="tags-field"><turboblog:list-link-tags>
							<a href="<turboblog:show-blog-permanent-link />/links?tagged=<turboblog:show-tag />" title="View all links tagged as <turboblog:show-tag />" rel="category tag"><turboblog:show-tag /></a><dsp:if test="last-link-tag?"><dsp:else>, </dsp:else></dsp:if></turboblog:list-link-tags>
						</span>
					</li>
				</ol>
			</fieldset>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
