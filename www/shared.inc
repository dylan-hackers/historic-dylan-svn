<?php
function begin_progress_table () {
    echo "<TABLE BORDER=0 CELLSPACING=10 WIDTH=\"100%\">\n";
}

function progress ($label, $count) {
    echo("<TR><TD><STRONG>" . $label . ":</STRONG></TD>" .
	 "<TD ALIGN=RIGHT><IMG SRC=\"images/progress" . $count . ".gif\" " .
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
<BODY TEXT="#000000" BGCOLOR="#FFFFFF" LINK="#x000099" ALINK="#xFF0000"
      VLINK="#x990000">
<H1 ALIGN=CENTER>
<?php echo "<IMG SRC=\"images/gwydion-logo.gif\" " .
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

<P><STRONG>About Gwydion</STRONG></P>
<UL>
<LI><A HREF="./">Overview</A>
<LI><A HREF="about-dylan.phtml">What is Dylan?</A>
<LI><A HREF="news.phtml">What's New</A>
<LI><A HREF="maintainers.phtml">Gwydion Maintainers</A>
<LI><A HREF="http://legend.gwydion.cs.cmu.edu/dylan-release/">Gwydion at
CMU</A>

<LI><A HREF="http://www.harlequin.com/devtools/dylan">Harlequin Dylan</a>, the
other implementation
<LI><A HREF="http://www.harlequin.com/support/dylan/bkshelf/bkshelf.htm">Other
Dylan Websites</A>
</UL>

<P><STRONG>Developer Resources</STRONG></P>
<UL>
<LI><A HREF="lists.phtml">Mailing List</A>
<LI><A HREF="downloading.phtml">Downloading Software</A>
<LI><A HREF="gdref.phtml">Documentation</A>
<LI><A HREF="http://www.ccc.de/cgi-bin/cvsweb/gd/">Browse CVS</A>
<LI><A HREF="bugs">Bug Tracker</A>
<LI><A HREF="bugs.private">Private Bug Tracker</A>
</UL>

<P><STRONG>Current Projects</STRONG></P>
<UL>
<LI><A HREF="ports.phtml">Ports</A>
<LI><A HREF="melange.phtml">C Interface</A>
<LI><A HREF="gui.phtml">GTK+ Interface</A>
<LI><A HREF="packaging.phtml">Packaging</A>
<LI><A HREF="documentation.phtml">Documentation</A>
</UL>

<P><STRONG>Wishlist</STRONG></P>
<UL>
<LI><A HREF="shared-libs.phtml">Shared Libraries</A>
<LI><A HREF="browser.phtml">GNU Dylan Browser!</A>
</UL>

</TD></TR>
</TABLE>

<DIV CLASS=footer>
<P><SMALL>Copyright &copy; 1998 Gwydion Dylan Maintainers. All rights reserved.
Created using <A HREF="http://www.php.net/">PHP</A>, <A
HREF="http://www.apache.org/">Apache</A> and <A HREF="http://www.postgresql.org/">PostgreSQL</A> on a machine running
<A HREF="http://www.redhat.com/">RedHat Linux</A>. Graphics by the <A HREF="http://www.gimp.org/">GIMP</A>.</SMALL></P>
</DIV>

</TD></TR>
</TABLE>

</BODY>
</HTML>
<?php
}

?>
