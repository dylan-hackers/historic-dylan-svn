<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="administration-meta-header.dsp"/>	
	<body>
	<%dsp:include url="administration-header.dsp"/>
	<%dsp:include url="administration-navigation.dsp"/>
		<div id="blog">
      <ul>
				<turboblog:list-users>
					<li>
						<strong>
							<a href="<turboblog:show-user-permanent-link />" title="<turboblog:show-user-username />">
								<turboblog:show-user-username />
							</a>
						</strong>
						<ul>
							<dsp:when test="edit-user-permitted?">
								<li>
									<a class="action" href="<turboblog:show-user-permanent-link />?edit" title="edit '<turboblog:show-user-username />'">
										edit
									</a>
								</li>
							</dsp:when>
							<dsp:when test="remove-user-permitted?">
								<li>
									<a class="action" href="<turboblog:show-user-permanent-link />?remove" title="remove '<turboblog:show-user-username />'">
										remove
									</a>
								</li>
							</dsp:when>
						</ul>
					</li>
				</turboblog:list-users>
			</ul>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
