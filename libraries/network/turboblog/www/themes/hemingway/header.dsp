<%dsp:taglib name="turboblog"/>
  <div class="header">
    <h1>
      <a href="<turboblog:show-blog-permanent-link />"><turboblog:show-blog-title/></a>
    </h1>
    <p class="description"><turboblog:show-blog-subtitle/></p>
    <dsp:when test="query-tagged?">
      <ul id="query-tags">
        <turboblog:list-query-tags>
          <li><turboblog:show-tag/></li>
        </turboblog:list-query-tags>
      </ul>
    </dsp:when>  
    <form method="get" id="search-form" action="<turboblog:show-blog-permanent-link />/archive">
      <ul>
        <li id="search-input"><input type="text" id="query" value="<turboblog:show-search/>" name="search" size="15" /></li>
      </ul>
    </form>
  </div>
