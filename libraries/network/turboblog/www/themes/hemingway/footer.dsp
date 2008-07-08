<%dsp:taglib name="turboblog"/>
<div class="footer">
	<ul>
		<li>
			Powered by <a href="http://turbolent.com/" title="turboblog"><span class="turboblog">turboblog</span></a>.
		</li>
		<li>
			<a class="button" href="<turboblog:show-blog-feed-url type="atom"/>">Atom</a>
		</li>
		<dsp:when test="editable?">
    	<li>
				<a class="button" href="?edit">edit</a>
			</li>
		</dsp:when>
		<dsp:when test="removable?">
			<li>
				<a class="button" href="?remove">remove</a>
			</li>
		</dsp:when>
		<dsp:when test="view-blog?">
			<dsp:when test="add-entry-permitted?">
				<li>
					<a class="button" href="<turboblog:show-blog-permanent-link />/archive?add">add</a>
				</li>
			</dsp:when>
			<dsp:when test="configure-blog-permitted?">
				<li>
					<a class="button" href="<turboblog:show-blog-permanent-link />?configure">configure</a>
				</li>
			</dsp:when>
		</dsp:when>
		<dsp:if test="authenticated?">
			<dsp:then>
				<li>
					<a class="button" href="<turboblog:show-logout-url redirect="#t" current="#t"/>">logout</a>
				</li>
			</dsp:then>
			<dsp:else>
				<li>
					<a class="button" href="<turboblog:show-login-url redirect="#t" current="#t"/>">login</a>
				</li>
			</dsp:else>
		</dsp:if>
	</ul>
</div> 
