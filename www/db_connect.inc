<?php

function db_connect() {
    $db = mysql_connect(":/var/run/mysqld/mysqld.sock",'www','');
    mysql_select_db("gwydion", $db);
    return $db;
}

?>
