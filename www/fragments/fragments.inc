<?php
ini_set("include_path", ini_get("include_path") . ":..");
require "shared.inc";

function code_header ($title) {
?>
<HTML>
<HEAD>
<TITLE>Dylan Examples: <?php echo $title?></TITLE>
</HEAD>
<BODY TEXT="#000000" BGCOLOR="#FFFFFF" LINK="#000099" ALINK="#FF0000"
      VLINK="#990000">
<H1 ALIGN=CENTER>
<IMG SRC="../images/gwydion-logo.gif" ALT="Gwydion Dylan">
</H1>

<P ALIGN=CENTER>[
<A HREF="../fragments.phtml">More examples</A> |
<A HREF="../">Overview</A>
]</P>

<TABLE BORDER=0 WIDTH="100%" CELLPADDING=10 CELLSPACING=0 BGCOLOR="#CCCCCC">
<TR><TD WIDTH="100%">

<?php
  echo "<H2>" . $title . "</H2>\n";
}

function code_footer () {
?>
</TD></TR>
</TABLE>
</TD>

<DIV CLASS=footer>

<P><SMALL>Copyright &copy; 1998 Gwydion Dylan Maintainers. All rights
reserved.  Created using <A HREF="http://www.php.net/">PHP</A>, <A
HREF="http://www.apache.org/">Apache</A> and <A
HREF="http://www.postgresql.org/">PostgreSQL</A> on a machine running <A
HREF="http://www.redhat.com/">RedHat Linux</A>. Graphics by the <A
HREF="http://www.gimp.org/">GIMP</A>. [<A
HREF="mailto:gd-bugs@randomhacks.com">Feedback</A>]</SMALL></P>

</DIV>
<?php
}
?>
