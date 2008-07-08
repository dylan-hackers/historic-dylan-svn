<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="meta-header.dsp"/>
	<body>
		<%dsp:include url="header.dsp"/>
		<div class="body">
			<div class="entry">
				<%dsp:include url="snippet-show-entry.dsp"/>
			</div>
			<hr class="separator"/>
		</div>
    <div class="ancillary">
      <div class="two">
        <h2>Comment</h2>
				<form action="<turboblog:show-entry-permanent-link />/comments?add" method="post">
					<fieldset>
						<ol>
							<li id="name">
								<label id="name-label" for="name-input">Name:</label>
								<dsp:if test="name-error?">
									<dsp:then>
										<input id="name-input" type="text" name="name" value="" />
										<span class="error">You have to enter a name!</span>
									</dsp:then>
									<dsp:else>
										<input id="name-input" type="text" name="name" value="<turboblog:show-comment-name />" />
									</dsp:else>
								</dsp:if>
							</li>
							<li id="website">
								<label id="website-label" for="website-input">Website:</label>
								<input id="website-input" type="text" name="website" value="<turboblog:show-comment-website/>"/>
							</li>
							<li id="body">
								<label id="body-label" for="body-text">Body:</label>
								<dsp:if test="body-error?">
									<dsp:then>
										<textarea id="body-text" name="body" cols="20" rows="5"></textarea>
										<span class="error">You have to enter a body!</span>
									</dsp:then>
									<dsp:else>
										<textarea id="body-text" name="body" cols="20" rows="5"><turboblog:show-comment-body /></textarea>
								 </dsp:else>
								</dsp:if>
							</li>
						</ol>
					</fieldset>
					<input type="hidden" name="on-error-page" value="view-entry-page" />
					<input type="submit" value="Comment" />
				</form>	
			</div>
      <div class="two">
        <h2>
					<dsp:if test="has-comments?"><dsp:then><turboblog:show-entry-comment-count /></dsp:then><dsp:else>No</dsp:else></dsp:if>
				  Comments
				</h2>
        <ol id="comments">
          <turboblog:list-entry-comments>
            <li id="comment-<turboblog:show-comment-comment-number/>">
              <cite> 
                <span class="author">
                  <dsp:if test="has-website?">
                    <dsp:then>
                      <a href="<turboblog:show-comment-website />" rel="external nofollow"><turboblog:show-comment-name/></a>
                    </dsp:then>
                    <dsp:else>
                      <turboblog:show-comment-name/>
                    </dsp:else>
                  </dsp:if>
                </span>
                <span class="date">
                  <turboblog:show-comment-published formatted="%e. %b %Y @ %H:%M"/>
                </span>
              </cite> 
              <div class="content">
                <turboblog:show-comment-body/>
              </div>
            </li>
          </turboblog:list-entry-comments>
        </ol>
				<dsp:when test="has-pingbacks?">
					<h2>
						Other opinions
					</h2>
        	<ol id="pingbacks">
						<turboblog:list-entry-pingbacks>
							<li id="pingback-<turboblog:show-pingback-pingback-number/>">
								<cite>
									<span class="title">
										<a href="<turboblog:show-pingback-pingback-source/>" rel="external nofollow">
											<turboblog:show-pingback-title/>
										</a>
									</span>
									<span class="date">
										<turboblog:show-pingback-published formatted="%e. %b %Y @ %H:%M"/>
									</span>
								</cite>
								<div class="content">
									<turboblog:show-pingback-body/>
								</div>	
							</li>
						</turboblog:list-entry-pingbacks>
        	</ol>
				</dsp:when>
      </div>
			<hr class="separator"/>
    </div>
		<%dsp:include url="footer.dsp"/>
	</body>
</html>
