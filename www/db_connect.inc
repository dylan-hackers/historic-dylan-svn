<?php

function db_connect() {
    $db = mysql_connect("localhost");
    mysql_select_db("gwydion", $db);
    return $db;
}

?>
