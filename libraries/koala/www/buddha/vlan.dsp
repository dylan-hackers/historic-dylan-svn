<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="buddha"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Sauron - VLAN</title>
  <link rel="stylesheet" href="/buddha/buddha.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <buddha:show-vlans output-format="html"/>
    <form action="/buddha/vlan.dsp" method="post">
      <div id="edit">
        VLAN: <input type="text" name="number" size=4>
        Name: <input type="text" name="name" size=40>
        Description: <input type="text" name="description">
        <input type="submit">
      </div>
    </form>
  </div>
</body>
</html>
