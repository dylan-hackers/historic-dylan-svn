<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/x
html1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="buddha"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Sauron - Hosts</title>
  <link rel="stylesheet" href="/buddha/buddha.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <buddha:show-hosts output-format="html"/>
    <form action="/buddha/host.dsp" method="post">
      <div id="edit">
        Name: <input type="text" name="name">
        IP: <input type="text" name="ip">
        Mac: <input type="text" name="mac">
        Zone: <buddha:option-zones>
        <input type="submit">
      </div>
    </form>
  </div>
</body>
</html>
