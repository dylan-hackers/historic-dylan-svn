<?php

require "db_connect.inc";

function current_version() {
    echo "2.4.0";
}

function begin_progress_table () {
    echo "<TABLE BORDER=0 CELLSPACING=10 WIDTH=\"100%\">\n";
}

function progress ($label, $count) {
    echo("<TR><TD><STRONG>" . $label . ":</STRONG></TD>" .
	 "<TD ALIGN=RIGHT><IMG SRC=\"/images/progress" . $count . ".png\" " .
	 "ALT=\"[" . $count . " of 10]\"></TD></TR>\n");
}

function end_progress_table () {
    echo "</TABLE>\n";
}

function short_header ($title) {
    readfile("/var/www/header.html");
    echo "<TITLE>Dylan: " . $title . "</TITLE><BODY>";
    readfile("/var/www/menu.html");
}

function standard_header ($title) {
    short_header($title);
    echo "<H2>" . $title . "</H2>\n";
}

function standard_footer () {
    readfile("/var/www/footer.html");
}
?>
