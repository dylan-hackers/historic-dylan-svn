<?php

require "db_connect.inc";

function current_version(){
    echo "2.3.10";
}

function begin_progress_table () {
    echo "<TABLE BORDER=0 CELLSPACING=10 WIDTH=\"100%\">\n";
}

function progress ($label, $count) {
    echo("<TR><TD><STRONG>" . $label . ":</STRONG></TD>" .
	 "<TD ALIGN=RIGHT><IMG SRC=\"images/progress" . $count . ".png\" " .
	 "ALT=\"[" . $count . " of 10]\"></TD></TR>\n");
}

function end_progress_table () {
    echo "</TABLE>\n";
}

function short_header ($title) {
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE>Gwydion Dylan: <?php echo $title?></TITLE>
</HEAD>
<BODY TEXT="#000000" BGCOLOR="#FFFFFF" LINK="#000099" ALINK="#FF0000"
      VLINK="#990000">
<H1 ALIGN=CENTER>
<?php echo "<IMG SRC=\"gwydion.png\" " .
           "ALT=\"Gwydion Dylan\">" ?>
</H1>

<P ALIGN=CENTER><STRONG>Advanced Free Software Development Tools</STRONG></P>

<TABLE BORDER=0 WIDTH="100%" CELLPADDING=0 CELLSPACING=10>

<TR>
<TD WIDTH="20%" VALIGN=TOP>

<TABLE BORDER=0 WIDTH="100%" CELLPADDING=10 CELLSPACING=0 BGCOLOR="#CCCCCC">
<TR><TD WIDTH="100%">

<form METHOD=GET ACTION="/cgi-bin/search.cgi">
<TABLE BORDER=0>
<TR><TD>
   <input TYPE="text" NAME="q" SIZE=20>
   </TD><TD>
<input TYPE="submit" VALUE="Search">
</TD></TR></TABLE>
</form>

<P><STRONG>About Dylan & Gwydion</STRONG></P>
<UL>
<LI><A HREF="./">Overview</A>
<LI><A HREF="about-dylan.phtml">What is Dylan?</A>
<LI><A HREF="about-gwydion.phtml">Who was Gwydion?</A>
<LI><A HREF="goals.phtml">Our Goals</A>
<LI><A HREF="contests.phtml">How does Dylan Stack Up?</A>
<LI><A HREF="ports.phtml">Supported Platforms</A>
<LI><A HREF="maintainers.phtml">Gwydion Maintainers</A>
<LI><A HREF="http://www.cs.cmu.edu/afs/cs/project/gwydion/docs/htdocs/gwydion/">Gwydion at CMU</A>
<LI><A HREF="http://www.fun-o.com">Functional Developer</a>, the commercial Dylan implementation
<LI><A HREF="news.phtml">Older News</A>
</UL>

<P><STRONG>User Resources</STRONG></P>
<UL>
<LI><A HREF="downloading.phtml">Downloading Software</A>
<LI><A HREF="gdref.phtml">Documentation</A>
<LI><A HREF="lists.phtml">Mailing Lists</A>
<LI><A HREF="http://monday.sourceforge.net/wiki/">Wiki</A>
<LI><A HREF="learning.phtml">Learning Dylan</A>
<LI><A HREF="users.phtml">Projects Using Dylan</A>
<LI><A HREF="screenshots.phtml">Screenshots</A>
<LI><A HREF="limitations.phtml">Current Limitations</A>
</UL>

<P><STRONG>Developer Resources</STRONG></P>
<UL>
<LI><A HREF="/cgi-bin/viewcvs.cgi">Browse CVS</A>
<LI><A HREF="/cgi-bin/gd">Bug Tracker</A>
<LI><A HREF="/cgi-bin/gd.private">Private Bug Tracker</A>
<LI><A HREF="projects.phtml">Current Projects</A>
<LI><A HREF="submit-news.phtml">Submit News</A>
</UL>

<P><STRONG>Wishlist</STRONG></P>
<UL>
<LI><A HREF="browser.phtml">Code Browser/IDE</A>
</UL>
</TD>
</TR>

<TD>
<TABLE>
<P><STRONG>CVS Activity</STRONG></P>
<P><SMALL>&nbsp;&nbsp;&nbsp;&nbsp;Count of lines recently <font color="#0000FF">added</font> and <font color="#FF0000">removed</font></SMALL></P>
<TR>
<A HREF="/cgi-bin/viewcvs.cgi">
<IMG SRC="cvsact.png" BORDER=0 ALT="CGI Activity" HEIGHT=220 WIDTH=238></A>
</TR></TABLE></TD>

<TR>
<TD>
   <DIV CLASS=footer>

<P><SMALL>Copyright &copy; 1998-2003 Gwydion Dylan Maintainers. All rights
reserved.  Created using <A HREF="http://www.php.net/">PHP</A>, <A
HREF="http://www.apache.org/">Apache</A> on a machine running <A
HREF="http://www.debian.org/">Debian Linux</A>. Graphics by the <A
HREF="http://www.gimp.org/">GIMP</A>. [<A
HREF="mailto:gd-hackers@gwydiondylan.org">Feedback</A>]</SMALL></P>

</DIV>
</TD></TR>
<tr><td><p style="font-size: 3pt; color: #cccccc">Spamtrap, please ignore:
<a style="color: #cccccc" href="http://homes.hinet.de/ohne_keys.htm">x</a>
<a style="color: #cccccc" href="http://homes.hinet.de/business_keys.htm">x</a>
<a style="color: #cccccc" href="http://homes.hinet.de/love_keys.htm">x</a>
<a style="color: #cccccc" href="http://homes.hinet.de/geld_keys.htm">x</a>
<a style="color: #cccccc" href="http://homes.hinet.de/privat_keys.htm">x</a>
</td></tr>
</TABLE>



<TD WIDTH="80%" VALIGN=TOP>

<TABLE BORDER=0 WIDTH="100%" CELLPADDING=10 CELLSPACING=0 BGCOLOR="#CCCCCC">
<TR><TD WIDTH="100%">
<?php
}

function standard_header ($title) {
    short_header($title);
    echo "<H2>" . $title . "</H2>\n";
}

function standard_footer () {
?>
</TD></TR>
</TABLE>
</TABLE>

</BODY>
</HTML>
<?php
}
?>
