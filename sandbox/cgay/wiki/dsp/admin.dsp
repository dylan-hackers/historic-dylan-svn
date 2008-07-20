<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%dsp:taglib name="wiki"/>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>Dylan Wiki: Administration</title>
  <link  rel="stylesheet" href="/wiki/wiki.css"/>
</head>

<body>
  <%dsp:include url="header.dsp"/>
  <dsp:show-form-notes/>
  <div id="content">
    <h1>Dylan Wiki Administration</h1>
    <wiki:privilege value="remove">
      <form action="/wiki/admin.dsp" method="post">
        <div id="edit">
          <select name="title" size="10">
            <wiki:show-index>
              <option><wiki:show-title/></option>
            </wiki:show-index>
          </select>
          <input type="hidden" name="action" value="remove">
          <input type="submit" value="Remove">
        </div>
      </form>
    </wiki:privilege>
    <wiki:privilege value="rename">
      <form action="/wiki/admin.dsp" method="post">
        <div id="edit">
          <select name="oldtitle" size="10">
            <wiki:show-index>
              <option><wiki:show-title/></option>
            </wiki:show-index>
          </select>
          <input type="hidden" name="action" value="rename">
          rename to <input type="text" name="title">
          <input type="submit" value="Rename"/>
        </div>
      </form>
    </wiki:privilege>
    <wiki:privilege value="undo">
      <form action="/wiki/admin.dsp" method="post">
        <div id="edit">
          <select name="title" size="10">
            <wiki:show-recent-changes count="20">
              <option><wiki:show-change-title/></option>
            </wiki:show-recent-changes>
          </select>
          <input type="hidden" name="action" value="undo">
          <input type="submit" value="revert">
        </div>
      </form>
    </wiki:privilege>
    <wiki:privilege value="change-privileges">
      <form action="/wiki/admin.dsp" method="post">
        <div id="edit">
          <table>
            <tr>
              <th>Username</th>
              <wiki:show-privileges>
                <th><wiki:show-privilege/></th>
              </wiki:show-privileges>
            </tr>
            <wiki:show-users>
              <tr>
                <td><wiki:show-user/></td>
                <wiki:show-privileges>
                  <td><input type="checkbox"
                             name="<wiki:show-user/>:<wiki:show-privilege/>"
                             <dsp:when test="privilege?">checked="checked"</dsp:when>/>
                  </td>
                </wiki:show-privileges>
              </tr>
            </wiki:show-users>
          </table>
          <input type="hidden" name="action" value="change-privileges">
          <input type="submit" value="Change privileges">
        </div>
      </form>
    </wiki:privilege>
    <wiki:privilege value="remove-user">
      <form action="/wiki/admin.dsp" method="post">
        <div id="edit">
          <select name="username" size="10">
            <wiki:show-users>
              <option><wiki:show-user/></option>
            </wiki:show-users>
          </select>
          <input type="hidden" name="action" value="remove-user">
          <input type="submit" value="remove">
        </div>
      </form>
    </wiki:privilege>
  </div>
  <%dsp:include url="footer.dsp"/>

</body>
</html>
