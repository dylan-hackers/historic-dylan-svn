<html>
<body>

This page demonstrates various features of Dylan Server Pages.<p>


<dsp:maybe-display-welcome/>

<!-- The most basic DSP tag.  No arguments, no body. -->
<p><hr>
Basic tags:<br>
<dsp:hello/>


<!-- A tag that has keyword arguments. -->
<p><hr>
Tags with keyword arguments:<br>
<dsp:show-keys arg1="a" arg2="b"/>


<!-- An iteration tag.  The body may be executed zero or more times. -->
<p><hr>
Iteration tags:<br>
Specify a query value of n=xxx in the URL to change the number of iterations.<p>
<dsp:repeat>
  This is iteration <dsp:display-iteration-number/>.<br>
</dsp:repeat>


<!-- Another iteration tag to demonstrate HTML table generation. -->
<p><hr>
Iteration tags used to dynamically generate tables:<br>
<table border="0" align="center" cellspacing="0" cellpadding="4">
  <dsp:no-iterations name="blah-table">
    <tr><td>There are no rows to display.</td></tr>
  </dsp:no-iterations>
  <dsp:iterator name="blah-table">
    <tr bgcolor="<dsp:row-bgcolor/>">
      <td width="10%"><dsp:iteration-number/></td>
      <td width="90%">blah blah blah</td>
    </tr>
  </dsp:iterator>
</table>
<p>
<table border="1">
  <dsp:no-iterations name="zero-table">
    <tr><td>This table has no rows to display.</td></tr>
  </dsp:no-iterations>
  <dsp:iterator name="zero-table">
    <tr bgcolor="<dsp:row-bgcolor/>">
      <td width="10%"><dsp:iteration-number/></td>
      <td width="90%">zero zero zero</td>
    </tr>
  </dsp:iterator>
</table>

<!-- The include directive -->
<p><hr>
Include directive:<br>
<%dsp:include url="/include-me.dsp"/>


<!-- A tag that shows the use of sessions. -->
<p><hr>
Using sessions to manage persistent data:<br>
The value below should change each time you refresh the page.<br>
<dsp:demo-sessions/>


</body>
</html>

