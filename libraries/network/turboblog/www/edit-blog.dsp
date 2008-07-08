<%dsp:taglib name="turboblog"/><?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<%dsp:include url="meta-header.dsp"/>	
	<body>
	<%dsp:include url="header.dsp"/>
  	<div class="body">
      <dsp:if test="editing?">
        <dsp:then>
          <form action="<turboblog:show-entry-permanent-link />?edit" method="post">
          <input type="hidden" name="original-title" value="<turboblog:show-entry-title />" />
        </dsp:then>
        <dsp:else>
          <form action="<turboblog:show-entry-permanent-link />?add" method="post">
        </dsp:else>
      </dsp:if>
        <dsp:if test="title-error?">
          <dsp:then>
            <h2><input class="error" id="title" name="title" value="" /></h2>
            <p class="error">You have to enter a title!</p>
          </dsp:then>
          <dsp:else>
            <h2><input id="title" name="title" value="<turboblog:show-entry-title />" /></h2>
          </dsp:else>
        </dsp:if>
        <textarea id="body" name="body" cols="70" rows="20"><turboblog:show-entry-body /></textarea>
        <div class="details">
          <span class="categories">Filed Under:
            <input id="tags" name="tags" value="<turboblog:list-entry-tags><turboblog:show-entry-tag /><dsp:if test="last-tag?"><dsp:else>, </dsp:else></dsp:if></turboblog:list-entry-tags>"/>
          </span>
        </div>
        <dsp:if test="editing?">
          <dsp:then>
            <input type="submit" value="Save" />
          </dsp:then>
          <dsp:else>
            <input type="submit" value="Post" />
          </dsp:else>
        </dsp:if>
      </form>
    </div>
		<%dsp:include url="footer.dsp"/>
	</body>
</html>
