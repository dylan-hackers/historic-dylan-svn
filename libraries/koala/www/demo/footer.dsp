<%dsp:taglib name="demo" prefix="demo"/>

<hr noshade width="90%" align="center">
<table width="90%" align="center">
  <tr>
    <td width="50%">
      <dsp:if test="logged-in?">
        <i>
        <dsp:then>You are logged in as <demo:current-username/>.</dsp:then>
        <dsp:else>You are not logged in.</dsp:else>
        </i>
      </dsp:if>
    </td>
    <td width="50%" align="right">
      <a href="home.dsp">Go back to demo home</a>
    </td>
  </tr>
</table>
