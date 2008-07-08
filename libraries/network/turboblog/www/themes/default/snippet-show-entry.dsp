<%dsp:taglib name="turboblog"/>
				<h3>
          <a href="<turboblog:show-blog-permanent-link />/archive/<turboblog:show-entry-update formatted="%Y"/>/"><turboblog:show-entry-update formatted="%Y"/></a>
          <a href="<turboblog:show-blog-permanent-link />/archive/<turboblog:show-entry-update formatted="%Y/%m"/>/"><turboblog:show-entry-update formatted="%m"/></a>
        	<a href="<turboblog:show-blog-permanent-link />/archive/<turboblog:show-entry-update formatted="%Y/%m/%d"/>/"><turboblog:show-entry-update formatted="%d"/></a>
				</h3>
        <h4>
					<a href="<turboblog:show-entry-permanent-link />" rel="bookmark" title="Permanent Link to '<turboblog:show-entry-title />'">
      			<turboblog:show-entry-title />
    			</a>
				</h4>
      	<div>
    			<turboblog:show-entry-body output-format="xhtml" escape="false"/>
  			</div> 
