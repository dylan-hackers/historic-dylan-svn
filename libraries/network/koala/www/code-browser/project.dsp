<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="code-browser"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title><code-browser:project-name/></title>
</head>

<body>
  <h1>Project: <code-browser:project-name/></h1>
  <h2>Library: <code-browser:project-library/></h2>
	<h2>Modules</h2>
	<code-browser:project-modules/>	
	<h2>Used libraries</h2>
	<code-browser:project-used-libraries/>
	<h2>Superclasses of &lt;string&gt;</h2>
	<code-browser:project-direct-superclasses/>
	<h2>Subclasses of &lt;string&gt;</h2>
	<code-browser:project-direct-subclasses/>
	<h2>Methods of concatenate</h2>
	<code-browser:generic-function-object-methods/>
	<h2>Source for method concatenate</h2>
	<code-browser:find-section-for-method/>
	<h2>Definition of generic concatenate</h2>
	<code-browser:find-section-for-definition/>
	<h2>Source Code</h2>
	<code-browser:project-sources/>
</body>
</html>
