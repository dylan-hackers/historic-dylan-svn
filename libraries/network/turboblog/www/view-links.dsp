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
      <ul>
				<turboblog:list-blog-links>
					<li>
						<strong>
							<a href="<turboblog:show-link-permanent-link />" title="<turboblog:show-link-title />">
								<turboblog:show-link-title />
							</a>
						</strong>
						<ul>
							<dsp:when test="edit-link-permitted?">
								<li>
									<a class="action" href="<turboblog:show-link-permanent-link />?edit" title="edit '<turboblog:show-link-title />'">
										edit
									</a>
								</li>
							</dsp:when>
							<dsp:when test="remove-link-permitted?">
								<li>
									<a class="action" href="<turboblog:show-link-permanent-link />?remove" title="remove '<turboblog:show-link-title />'">
										remove
									</a>
								</li>
							</dsp:when>
						</ul>
					</li>
				</turboblog:list-blog-links>
			</ul>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
