<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <%dsp:include url="administration-meta-header.dsp"/>
	<body>
		<%dsp:include url="administration-header.dsp"/>
		<%dsp:include url="administration-navigation.dsp"/>
  	<div class="entry">
      <form action="?<dsp:if test="edit-entry?"><dsp:then>edit</dsp:then><dsp:else>add</dsp:else></dsp:if>" method="post">
				<fieldset>
					<ol>
						<li id="title">
							<label id="title-label" for="title-input">Title:</label>
							<input id="title-input" type="text" name="title" value="<turboblog:show-entry-title />" />
							<dsp:when test="title-error?">
								<span class="error">You have to enter a title!</span>
							</dsp:when>
							<dsp:when test="entry-exists-already-error?">
								<span class="error">An entry with this title was already created.</span>
							</dsp:when>
						</li>
						<li id="type">
							<label id="type-label" for="type-select">Format</label>
							<select id="type-select" name="type">
								<turboblog:list-supported-content-types>
									<option<dsp:when test="entry-content-type?"> selected="selected"</dsp:when> value="<turboblog:show-content-type-name/>"><turboblog:show-content-type/></option>
								</turboblog:list-supported-content-types>
							</select>
							<dsp:when test="type-error?">
								<span class="error">Currently supported types are only XHTML and Markup!</span>
							</dsp:when>
						</li>
			 			<li id="body">  
              <label id="body-label" for="body-text">Body:</label>
              <dsp:if test="body-error?">
                <dsp:then>
                  <textarea id="body-text" name="body" cols="70" rows="20"></textarea>
                  <span class="error">You have to enter a body!</span>
                </dsp:then>
                <dsp:else>
                  <textarea id="body-text" name="body" cols="70" rows="20"><turboblog:show-entry-body escape="true"/></textarea>
               </dsp:else>   
              </dsp:if>
            </li>
						<li id="published">
							<label id="publishing-date-label" for="publishing-date-input">Publishing date:</label>
							<input id="publishing-date-input" type="text" name="publishing-date" value="<turboblog:show-entry-published formatted="%e. %b %Y @ %H:%M"/>"/>
						</li>
						<li id="tags">
							<label id="tags-label" for="tags-input">Tags:</label>
							<input id="tags-input" type="text" name="tags" value="<turboblog:list-entry-tags><turboblog:show-tag /> </turboblog:list-entry-tags>"/>
						</li>
					</ol>
				</fieldset>
				<input type="submit" value="<dsp:if test="edit-entry?"><dsp:then>Save</dsp:then><dsp:else>Post</dsp:else></dsp:if>" />
			</form>
    </div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
