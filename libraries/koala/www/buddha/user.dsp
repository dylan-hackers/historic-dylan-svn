<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="buddha"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Sauron - My Host</title>
  <link rel="stylesheet" href="/buddha/buddha.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <form action="/buddha/user.dsp" method="post">
      <div id="edit">
        Name: <input type="text" name="name">.<buddha:get-zone>
        Mac: <input type="text" name="mac">
        <input type="submit">
      </div>
    </form>
  </div>
</body>
</html>
