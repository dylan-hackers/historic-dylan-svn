<?php

function db_connect() {
    $db = mysql_connect(":/tmp/mysql.sock",'www','');
    mysql_select_db("gwydion", $db);
    return $db;
}

?>
