<%dsp:taglib name="turboblog"/>
	<ul id="sub-navigation">
		<dsp:when test="configure-blog-permitted?">
			<li<dsp:when test="configure-blog?"> class="active"</dsp:when>>
				<a href="<turboblog:show-blog-permanent-link />?configure">Configuration</a>
			</li>
		</dsp:when>
		<li>
      <ul>
				<dsp:when test="list-participants-permitted?">
        	<li <dsp:when test="list-participants?">class="active"</dsp:when>>
          	<a href="<turboblog:show-blog-permanent-link />/participants" title="list participants">Participants</a>
        	</li>
				</dsp:when>
				<dsp:when test="add-participant-permitted?">
        	<li <dsp:when test="add-participant?">class="active"</dsp:when>>
          	<a href="<turboblog:show-blog-permanent-link />/participants?add" title="add participants">+</a>
        	</li>
				</dsp:when>
        <dsp:when test="participant?">
          <li class="active"><a name="participant"><turboblog:with-participant-user><turboblog:show-user-username/></turboblog:with-participant-user></a></li>
        </dsp:when>
      </ul>
    </li>
		<li>
      <ul>
				<dsp:when test="list-links-permitted?">
    	    <li <dsp:when test="list-links?">class="active"</dsp:when>>
						<a href="<turboblog:show-blog-permanent-link />/links" title="list links">Links</a>
					</li>
      	</dsp:when>
				<dsp:when test="add-link-permitted?">
					<li <dsp:when test="add-link?">class="active"</dsp:when>>
						<a href="<turboblog:show-blog-permanent-link />/links?add" title="add link">+</a>
					</li>
				</dsp:when>
        <dsp:when test="link?">
          <li class="active"><a name="link"><turboblog:show-link-title/></a></li>
        </dsp:when>
      </ul>
    </li>
		<li>
			<ul>
				<dsp:when test="list-files-permitted?">
	        <li <dsp:when test="list-files?">class="active"</dsp:when>>
  	        <a href="<turboblog:show-blog-permanent-link />/files" title="list files">Files</a>
    	    </li>
				</dsp:when>
				<dsp:when test="add-file-permitted?">
	        <li <dsp:when test="add-file?">class="active"</dsp:when>>
  	        <a href="<turboblog:show-blog-permanent-link />/files?add" title="add file">+</a>
    	    </li>
				</dsp:when>
        <dsp:when test="file?">
					<li class="active"><a name="file"><turboblog:show-file-filename/></a></li>
				</dsp:when>
      </ul>
    </li>	
	</ul>
