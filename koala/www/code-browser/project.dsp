<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="code-browser"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title><code-browser:project-name/></title>
</head>

<body>
  <div id="content">
    <div id="name">
	<h1>Browsing project <emph><code-browser:project-name/></emph>.</h1>
    </div>
    <div id="used-libraries">
	<h2>Used libraries</h2>
	<code-browser:project-used-libraries/>
    </div>
    <div id="source">
      <h2>Source Code</h2>
      <code-browser:project-sources/>
    </div>
  </div>
</body>
</html>
