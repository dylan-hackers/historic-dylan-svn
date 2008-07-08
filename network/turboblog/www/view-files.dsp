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
				<turboblog:list-blog-files>
					<li>
						<strong>
							<a href="<turboblog:show-blog-permanent-link />/files/<turboblog:show-file-filename/>" title="<turboblog:show-file-filename />">
								<turboblog:show-file-filename />
							</a>
						</strong>
						<ul>
							<dsp:when test="edit-file-permitted?">
								<li>
									<a class="action" href="<turboblog:show-blog-permanent-link />/files/<turboblog:show-file-filename/>?edit" title="edit '<turboblog:show-file-filename />'">
										edit
									</a>
								</li>
							</dsp:when>
							<dsp:when test="remove-file-permitted?">
								<li>
									<a class="action" href="<turboblog:show-blog-permanent-link />/files/<turboblog:show-file-filename/>?remove" title="remove '<turboblog:show-file-filename/>'">
										remove
									</a>
								</li>
							</dsp:when>
						</ul>
					</li>
				</turboblog:list-blog-files>
			</ul>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
