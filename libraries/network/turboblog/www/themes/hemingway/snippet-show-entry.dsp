<%dsp:taglib name="turboblog"/>
	<h2>
		<a href="<turboblog:show-entry-permanent-link />" rel="bookmark" title="Permanent Link to '<turboblog:show-entry-title />'">
			<turboblog:show-entry-title />
		</a>
	</h2>
	<div>
		<turboblog:show-entry-body output-format="xhtml" escape="false"/>
	</div>
	<div class="details">
		<dsp:when test="blog-show-entry-authors?">
			<span class="authors">by 
				<turboblog:list-entry-authors>
					<turboblog:with-participant-user>
						<a href="<turboblog:show-user-permanent-link/>"><turboblog:show-user-username /></a><dsp:if test="last-entry-author?"><dsp:else>, </dsp:else></dsp:if>
					</turboblog:with-participant-user>
				</turboblog:list-entry-authors>
			</span>
		</dsp:when>
		<span class="date"><turboblog:show-entry-update formatted="%e. %b %Y @ %H:%M"/></span>
		<span class="comments">
			<a href="<turboblog:show-entry-permanent-link />#comments" title="Comment on '<turboblog:show-entry-title />'">
				<dsp:if test="has-comments?"><dsp:then><turboblog:show-entry-comment-count /></dsp:then><dsp:else>no</dsp:else></dsp:if>
				comments
			</a>
    </span>
		<dsp:if test="has-tags?">
			<dsp:then>
				<span class="categories">tagged as
					<turboblog:list-entry-tags><a href="<turboblog:show-blog-permanent-link/>/archive?tagged=<turboblog:show-tag />" title="View all posts tagged as <turboblog:show-tag />" rel="category tag"><turboblog:show-tag /></a><dsp:if test="last-entry-tag?"><dsp:else>, </dsp:else></dsp:if></turboblog:list-entry-tags>
				</span>
			</dsp:then>
		</dsp:if>
	</div>
