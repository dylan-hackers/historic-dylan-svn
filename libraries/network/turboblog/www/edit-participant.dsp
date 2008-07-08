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
			<dsp:if test="edit-participant?">
	    	<dsp:then>
					<form action="<turboblog:show-participant-permanent-link />?edit" method="post">
				</dsp:then>
				<dsp:else>
					<form action="<turboblog:show-blog-permanent-link />/participants?add" method="post">
				</dsp:else>
			</dsp:if>	
				<fieldset>
					<legend>General</legend>
					<ol>
						<li id="user">
              <label id="user-label" for="user-select">User <em>*</em></label>
              <dsp:if test="add-participant?">
                <dsp:then>
							    <select id="user-select" name="user">              
								    <turboblog:list-possible-participants>
									    <option><turboblog:show-user-username /></option>
								    </turboblog:list-possible-participants>
							    </select>
                </dsp:then>
                <dsp:else>
									<turboblog:with-participant-user>
                  	<span id="user-field"><turboblog:show-user-username /></span>
                	</turboblog:with-participant-user>
								</dsp:else>
              </dsp:if>
							<dsp:when test="user-error?"><span class="error">A user is required.</span></dsp:when>
							<dsp:when test="participant-exists-already-error?"><span class="error">This user is already a participant.</span></dsp:when>
						</li>
						<li id="role">
              <label id="role-label" for="role-select">Role <em>*</em></label>
              <select id="role-select" name="role">
								<turboblog:list-roles>
									<option<dsp:when test="participant-role?"> selected="selected"</dsp:when>><turboblog:show-role /></option>
								</turboblog:list-roles>
							</select>
              <dsp:when test="role-error?"><span class="error">A role is required.</span></dsp:when>
							<dsp:when test="unknown-role-error?"><span class="error">This role is unknown.</span></dsp:when>
            </li>
					</li>
				</ol>
				</fieldset>
				<dsp:if test="edit-participant?">
					<dsp:then>
						<input type="submit" value="Save"/>
					</dsp:then>
					<dsp:else>
						<input type="submit" value="Add"/>
					</dsp:else>
				</dsp:if>
			</form>
		</div>
		<%dsp:include url="administration-footer.dsp"/>
	</body>
</html>
