<%dsp:taglib name="example" prefix="ex"/>

<hr noshade width="90%" align="center">
<table width="90%" align="center">
  <tr>
    <td width="50%">
      <ex:iff test="logged-in?">
        <i>You are logged in as <ex:current-username/></i>
      </ex:iff>
    </td>
    <td width="50%" align="right">
      <a href="home.dsp">Go back to demo home</a>
    </td>
  </tr>
</table>
