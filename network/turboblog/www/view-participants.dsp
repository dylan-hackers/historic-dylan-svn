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
				<turboblog:list-blog-participants>
					<turboblog:with-participant-user>
					<li>
						<strong>
							<a href="<turboblog:show-participant-permanent-link />" title="<turboblog:show-user-username />">
								<turboblog:show-user-username />
							</a>
						</strong>
						<ul>
							<dsp:when test="edit-participant-permitted?">
								<li>
									<a class="action" href="<turboblog:show-participant-permanent-link />?edit" title="edit '<turboblog:show-user-username />'">
										edit
									</a>
								</li>
							</dsp:when>
							<dsp:when test="remove-participant-permitted?">
								<li>
									<a class="action" href="<turboblog:show-participant-permanent-link />?remove" title="remove '<turboblog:show-user-username />'">
										remove
									</a>
								</li>
							</dsp:when>
						</ul>
					</li>
					</turboblog:with-participant-user>
				</turboblog:list-blog-participants>
			</ul>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
