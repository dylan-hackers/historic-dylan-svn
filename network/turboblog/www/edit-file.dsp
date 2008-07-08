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
			<dsp:if test="edit-file?">
	    	<dsp:then>
					<form action="<turboblog:show-blog-permanent-link />/files/<turboblog:show-file-filename />?edit" method="post" enctype="multipart/form-data">
				</dsp:then>
				<dsp:else>
					<form action="<turboblog:show-blog-permanent-link />/files?add" method="post" enctype="multipart/form-data">
				</dsp:else>
			</dsp:if>	
				<fieldset id="general">
					<legend>General</legend>
					<ol>
						<dsp:when test="add-file?">
              <li id="file">
                <label id="file-label" for="file-input">File <em>*</em></label>
                <input id="file-input" type="file" name="file" />
                <dsp:when test="file-error?"><span class="error">A file is required.</span></dsp:when>
              </li>
            </dsp:when>
						<li id="filename">
							<label id="filename-label" for="filename-input">Filename <dsp:when test="edit-file?"><em>*</em></dsp:when></label>
							<input id="filename-input" type="text" name="filename" value="<turboblog:show-file-filename />"/>
							<dsp:when test="filename-error?"><span class="error">An filename is required.</span></dsp:when>
							<dsp:when test="file-exists-already-error?"><span class="error">There&quot;s already a file with this name.</span></dsp:when>
						</li>
					</ol>
				</fieldset>
				<dsp:if test="edit-file?">
					<dsp:then>
						<input type="submit" value="Save"/>
					</dsp:then>
					<dsp:else>
						<input type="submit" value="Upload"/>
					</dsp:else>
				</dsp:if>
			</form>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
