Module: btrack
Author: Carl Gay


// ---TODO: Connection pools.

define macro with-database-connection
  { with-database-connection (?conn:name) ?:body end }
  => { let _connection = #f;
       block ()
         let _conn :: sql$<connection> = open-connection("btrack");
         _connection := _conn;
         let ?conn :: sql$<connection> = _conn;
         sql$with-connection (_connection)  // sets up *default-connection* only
           ?body
         end
       cleanup
         _connection & close-connection(_connection);
       end;
     }
end;

define method open-connection
    (database :: <string>,
     #key user-name :: false-or(<string>) = #f,
          password :: false-or(<string>) = #f)
 => (connection :: sql$<odbc-connection>)
  let dbms = make(sql$<odbc-dbms>);
  sql$with-dbms(dbms)
    let user = make(sql$<user>, user-name: user-name | "", password: password | "");
    let db = make(sql$<database>, datasource-name: database);
    sql$connect(db, user)
  end
end;

define method query-database
    (connection :: sql$<connection>, query :: <byte-string>) 
 => (rows :: <sequence>)
  sql$with-connection (connection)
    let statement = make(sql$<sql-statement>, text: query);
    let result-set = sql$execute(statement);
    let results = map-as(<simple-object-vector>, identity, result-set);
    results
  end;
end;

define method close-connection (connection :: sql$<connection>) => ()
  sql$disconnect(connection)
end;

