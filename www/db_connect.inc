<?php

function db_connect() {
    $db = mysql_connect("localhost","gwydion","");
    mysql_select_db("gwydion", $db);
    return $db;
}

?>