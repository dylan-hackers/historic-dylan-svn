<?php

require "db_connect.inc";

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
<TR><TD WIDTH="70%" VALIGN=TOP>

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
</TD>

<TD WIDTH="30%" VALIGN=TOP>

<TABLE BORDER=0 WIDTH="100%" CELLPADDING=10 CELLSPACING=0 BGCOLOR="#CCCCCC">
<TR><TD WIDTH="100%">

<P><STRONG>About Dylan & Gwydion</STRONG></P>
<UL>
<LI><A HREF="./">Overview</A>
<LI><A HREF="about-dylan.phtml">What is Dylan?</A>
<LI><A HREF="news.phtml">What's New?</A>
<LI><A HREF="goals.phtml">Our Goals</A>
<LI><A HREF="ports.phtml">Supported Platforms</A>
<LI><A HREF="maintainers.phtml">Gwydion Maintainers</A>
<LI><A HREF="http://www.cs.cmu.edu/afs/cs/project/gwydion/docs/htdocs/gwydion/">Gwydion at CMU</A>
<LI><A HREF="http://www.fun-o.com">Functional Developer</a>, the commercial Dylan implementation
</UL>

<P><STRONG>User Resources</STRONG></P>
<UL>
<LI><A HREF="screenshots.phtml">Screenshots</A>
<LI><A HREF="limitations.phtml">Current Limitations</A>
<LI><A HREF="downloading.phtml">Downloading Software</A>
<LI><A HREF="lists.phtml">Mailing Lists</A>
<LI><A HREF="learning.phtml">Learning Dylan</A>
<LI><A HREF="gdref.phtml">Documentation</A>
</UL>

<P><STRONG>Developer Resources</STRONG></P>
<UL>
<LI><A HREF="http://berlin.ccc.de/cgi-bin/cvsweb/gd/">Browse CVS</A>
<LI><A HREF="http://www.gwydiondylan.org/cgi-bin/gd">Bug Tracker</A>
<LI><A HREF="http://www.gwydiondylan.org/cgi-bin/gd.private">Private Bug Tracker</A>
<LI><A HREF="projects.phtml">Current Projects</A>
</UL>

<P><STRONG>Wishlist</STRONG></P>
<UL>
<LI><A HREF="browser.phtml">Code Browser/IDE</A>
</UL>

</TD></TR>
</TABLE>

<DIV CLASS=footer>

<P><SMALL>Copyright &copy; 1998-2000 Gwydion Dylan Maintainers. All rights
reserved.  Created using <A HREF="http://www.php.net/">PHP</A>, <A
HREF="http://www.apache.org/">Apache</A> on a machine running <A
HREF="http://www.jurix.org/">Jurix Linux</A>. Graphics by the <A
HREF="http://www.gimp.org/">GIMP</A>. [<A
HREF="mailto:gd-hackers@gwydiondylan.org">Feedback</A>]</SMALL></P>

</DIV>

</TD></TR>
</TABLE>

</BODY>
</HTML>
<?php
}
?>
