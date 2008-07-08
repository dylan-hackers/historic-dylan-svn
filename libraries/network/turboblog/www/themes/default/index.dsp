<%dsp:taglib name="turboblog"/><?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html>
	<head>
		<title><turboblog:show-blog-title/> - blog<dsp:when test="entry?"> &ndash; <turboblog:show-entry-title/></dsp:when></title>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
		<link rel="stylesheet" href="<turboblog:show-blog-permanent-link />/style.css" type="text/css" media="screen"/>
		<link rel="alternate" type="application/atom+xml" title="<turboblog:show-blog-title/>" href="<turboblog:show-blog-feed-url type="atom"/>" />
		<link rel="pingback" href="<turboblog:show-blog-server-url/>/RPC2" />
	</head>
	<body>
	  <h1><a href="<turboblog:show-blog-permanent-link />"><turboblog:show-blog-title/></a></h1>
    <h2><turboblog:show-blog-subtitle/></h2>
    <ul id="navigation">
      <li><a href="<turboblog:show-blog-permanent-link />/about">about</a></li>
      <li class="active"><a href="<turboblog:show-blog-permanent-link />">blog</a></li>
      <li><a href="<turboblog:show-blog-permanent-link />/projects">projects</a></li>
    </ul>
    <div id="content">
			<ul id="entries">
			<turboblog:list-recent-entries last-entry="5">
    	  <li>
					<%dsp:include url="snippet-show-entry.dsp"/>
	      </li>
			</turboblog:list-recent-entries>
    	</ul>
		</div>
	</body>
</html>
