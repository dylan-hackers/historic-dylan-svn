<?php

require "db_connect.inc";

function current_version(){
    echo "2.3.11";
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
    <LINK REL="stylesheet" TYPE="text/css" HREF="sinorca-grey.css" TITLE="Grey boxes stylesheet" />
    <LINK rel="stylesheet alternative" TYPE="text/css" HREF="sinorca-white.css" title="White boxes stylesheet" />
    <LINK rel="stylesheet" TYPE="text/css" HREF="color-scheme.css" />
    <TITLE>Gwydion Dylan: <?php echo $title?></TITLE>
</HEAD>
<BODY>
    <DIV ID="top"></DIV>

    <div id="upperMenuBar">
      <div class="LHS">
        <a href="index.phtml" class="highlight">Home</a>
      </div>
      <div class="RHS">
        <a href="about-dylan.phtml">About Dylan</a>|
        <a href="downloading.phtml">Downloads</a>|
        <a href="gdref.phtml">Documentation</a>|
        <a href="lists.phtml">Mailing Lists</a>|
        <a href="http://monday.sourceforge.net/wiki/">Wiki</a>
      </div>
    </div>

    <DIV ID="header"><img src="gwydion-8caae6.png"></DIV>

    <form id="headerSearch" action="/cgi-bin/search.cgi" method="get">
      <div>
        <input type="text" class="text" name="q" value="Search..." />
      
        <!-- ###### Dynamic text box version:
        <input class="text" type="text" value="Search..."
               onFocus="if (this.value == 'Search...') this.value=''; else this.select();"
               onBlur="if (this.value == '') this.value='Search...';" />
             ###### -->

        <input type="submit" class="submit" value="GO" />
      </div>
    </form>

    <div class="sideMenuBox">
      <div>About Dylan &amp; Gwydion</div>
      <A HREF="about-dylan.phtml">&rsaquo; What is Dylan?</A>
      <A HREF="about-gwydion.phtml">&rsaquo; Who was Gwydion?</A>
      <A HREF="goals.phtml">&rsaquo; Our Goals</A>
      <A HREF="contests.phtml">&rsaquo; Dylan Competes</A>
      <A HREF="ports.phtml">&rsaquo; Supported Platforms</A>
      <A HREF="maintainers.phtml">&rsaquo; Gwydion Maintainers</A>
      <A HREF="http://www.cs.cmu.edu/afs/cs/project/gwydion/docs/htdocs/gwydion/">&raquo; Gwydion at CMU</A>
      <A HREF="http://www.fun-o.com">&raquo; Functional Developer</a>
      <A HREF="news.phtml">&rsaquo; Older News</A>
    </div>

    <div class="sideMenuBox lighterBG">
      <div>User Resources</div>
      <A HREF="downloading.phtml">&rsaquo; Downloading Software</A>
      <A HREF="gdref.phtml">&rsaquo; Documentation</A>
      <A HREF="lists.phtml">&rsaquo; Mailing Lists</A>
      <A HREF="gd-bugs">&rsaquo; Bug Tracker</A>
      <A HREF="http://monday.sourceforge.net/wiki/">&raquo; Wiki</A>
      <A HREF="learning.phtml">&rsaquo; Learning Dylan</A>
      <A HREF="users.phtml">&rsaquo; Projects Using Dylan</A>
      <A HREF="screenshots.phtml">&rsaquo; Screenshots</A>
      <A HREF="limitations.phtml">&rsaquo; Current Limitations</A>
    </div>

    <div class="sideMenuBox">
      <div>Developer Resources</div>
      <A HREF="/cgi-bin/viewcvs.cgi">&rsaquo; Browse CVS</A>
      <A HREF="/cgi-bin/gd.private">&rsaquo; Private Bug Tracker</A>
      <A HREF="projects.phtml">&rsaquo; Current Projects</A>
      <A HREF="submit-news.phtml">&rsaquo; Submit News</A>
    </div>
<!--
    <div class="sideTextBox">
      <div>CVS Activity</div>
<P><SMALL>&nbsp;&nbsp;&nbsp;&nbsp;Count of lines recently <font color="#0000FF">added</font> and <font color="#FF0000">removed</font></SMALL></P>
<TR>
<A HREF="/cgi-bin/viewcvs.cgi">
<IMG SRC="/cvsact.png" BORDER=0 ALT="CGI Activity" HEIGHT=220 WIDTH=238></A>
    </div>
-->

    <div id="bodyText">
<?php
}

function standard_header ($title) {
    short_header($title);
    echo "<H2>" . $title . "</H2>\n";
}

function standard_footer () {
?>
    </div>
    
    <!-- ###### Footer ###### -->

    <div id="footer">
      <div>
        <span class="footerLHS">
          [<a href="mailto:gd-hackers@gwydiondylan.org">Feedback</a>]
        </span>
      </div>

      <div>
Copyright &copy;1998&ndash;2003 Gwydion Dylan Maintainers. All rights
reserved.
      </div>

      <div>  Created using <A HREF="http://www.php.net/">PHP</A>, <A
HREF="http://www.apache.org/">Apache</A> on a machine running <A
HREF="http://www.debian.org/">Debian Linux</A>. Graphics by the <A
HREF="http://www.gimp.org/">GIMP</A>.
      </div>
    </div>
  </body>
</html>
<?php
}
?>
