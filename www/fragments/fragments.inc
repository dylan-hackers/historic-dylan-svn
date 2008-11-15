<?php
ini_set("include_path", ini_get("include_path") . ":..");
require "shared.inc";

function short_code_header ($title) {
   readfile("/var/www/website/header.html");
   echo "<title>12 Dylan Fragments: " . $title . "</title><body>";
   readfile("/var/www/website/menu.html");
}

function code_header ($title) {
   short_code_header($title);
?>
<p align=center>[
<a href="../fragments.phtml">All Fragments</a>
]</p>

<?php
  echo "<h2>" . $title . "</h2>\n";
}

function code_footer () {
   standard_footer();
}
?>
