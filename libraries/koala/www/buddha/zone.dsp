<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<%dsp:taglib name="buddha"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Sauron - Zones</title>
  <link rel="stylesheet" href="/buddha/buddha.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <div id="content">
    <buddha:show-zones output-format="html"/>
    <form action="/buddha/zone.dsp" method="post">
      <div id="edit">
        Name: <input type="text" name="name"><br>
        Hostmaster: <input type="text" name="hostmaster"><br>
        Serial: <input type="text" name="serial"><br>
        Refresh: <input type="text" name="refresh"><br>
        Retry: <input type="text" name="retry"><br>
        Expire: <input type="text" name="expire"><br>
        Time to live: <input type="text" name="time-to-live"><br>
        Nameserver: <input type="text" name="Namerserver"><br>
        Mail Exchange: <input type="text" name="mail-exchange"><br>
        TXT: <input type="text" name="txt"><br>
        <input type="submit">
      </div>
    </form>
  </div>
</body>
</html>
