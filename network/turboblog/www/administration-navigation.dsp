<%dsp:taglib name="turboblog"/>
	<ul id="navigation">
		<dsp:when test="administrate-permitted?">
	 		<li <dsp:when test="administrate?">class="active"</dsp:when>><a href="/?administrate">Administrate</a></li>
		</dsp:when>
		<li>
			<ul>
				<li <dsp:when test="list-blogs?">class="active"</dsp:when>><a href="/blogs" title="list blogs">Blogs</a></li>
				<dsp:when test="add-blog-permitted?">
					<li <dsp:when test="add-blog?">class="active"</dsp:when>><a href="/blogs?add" title="add blog">+</a></li>
				</dsp:when>
				<dsp:when test="blog?">
					<li class="active"><a href="<turboblog:show-blog-permanent-link />"><turboblog:show-blog-title/></a></li>
				</dsp:when>
			</ul>
		</li>
		<li>
			<ul>
				<li <dsp:when test="list-users?">class="active"</dsp:when>><a href="/users" title="list users">Users</a></li>
				<li <dsp:when test="add-user?">class="active"</dsp:when>><a href="/users?add" title="add user">+</a></li>
				<dsp:when test="user?">
					<li class="active"><a href="<turboblog:show-user-permanent-link />"><turboblog:show-user-username/></a></li>
				</dsp:when>
			</ul>
		</li>
		<li class="user">
			<dsp:if test="authenticated?">
				<dsp:then>	
					<turboblog:with-authenticated-user>
						<a href="<turboblog:show-user-permanent-link/>"><turboblog:show-user-username/></a> 
						&mdash;
            <a href="<turboblog:show-logout-url redirect="#t" current="#t"/>">logout</a>
					</turboblog:with-authenticated-user>
				</dsp:then>
				<dsp:else>
					<a href="<turboblog:show-login-url redirect="#t" current="#t"/>">login</a>
				</dsp:else>
			</dsp:if>
		</li>
	</ul>
