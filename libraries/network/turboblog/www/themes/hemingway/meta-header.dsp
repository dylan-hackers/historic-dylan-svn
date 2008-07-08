<%dsp:taglib name="turboblog"/>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title><turboblog:show-blog-title/><dsp:when test="entry?"> &ndash; <turboblog:show-entry-title/></dsp:when></title>
    <link rel="stylesheet" href="<turboblog:show-blog-permanent-link />/style.css" type="text/css" media="screen" />
    <link rel="alternate" type="application/atom+xml" title="<turboblog:show-blog-title/> Atom feed" href="<turboblog:show-blog-feed-url type="atom"/>" />
    <link rel="pingback" href="<turboblog:show-blog-server-url/>/RPC2" />
  </head>

