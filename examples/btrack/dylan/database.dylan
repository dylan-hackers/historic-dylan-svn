Module: btrack
Author: Carl Gay


// ---TODO: Connection pools.

define constant $database-name :: <string> = "dybug";

define macro with-database-connection
  { with-database-connection (?conn:name) ?:body end }
  => { let _connection = #f;
       block ()
         let _conn :: sql$<connection> = open-connection($database-name);
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

define method query-db
    (connection :: sql$<connection>, query-format-string :: <byte-string>, #rest format-args)
 => (rows :: <sequence>)
  sql$with-connection (connection)
    let statement = make(sql$<sql-statement>,
                         text: apply(format-to-string, query-format-string, format-args));
    let result-set = sql$execute(statement);
    let results = map-as(<simple-object-vector>, identity, result-set);
    results
  end;
end;

// This may be used for updates and inserts.
//
define method update-db
    (connection :: sql$<connection>, query :: <byte-string>, #rest parameters)
  sql$with-connection (connection)
    let statement = make(sql$<sql-statement>, text: query);
    sql$execute(statement, parameters: parameters);
  end;
end;

define method close-connection (connection :: sql$<connection>) => ()
  sql$disconnect(connection)
end;


////
//// Record IDs
////

define constant $record-id-lock = make(<lock>);
define constant $record-id-batch-size :: <integer> = 100;
define variable *record-id-next-batch-start* :: <integer> = 0;
define variable *next-record-id* :: <integer> = 0;

// Returns the next unique identifier to be used as a record_id.
//
define function next-record-id
    () => (uid :: <integer>)
  with-lock ($record-id-lock)
    while (*next-record-id* >= *record-id-next-batch-start*)
      load-next-record-id();
    end;
    inc!(*next-record-id*);
    *next-record-id*
  end;
end;

// Called when the current batch of record IDs has been exhausted.
// Updates the database with the new max and returns the next record id.
//
define function load-next-record-id
    () => (id :: <integer>)
  with-database-connection (conn)
    let rset = query-db(conn, "select next_record_id from tbl_config");
    let next-id = rset[0];
    let next-batch-start = next-id + $record-id-batch-size;
    update-db(conn, "update tbl_config set next_record_id = ?", next-batch-start);
    // Don't set these until update successful.
    *record-id-next-batch-start* := next-batch-start;
    *next-record-id* := next-id;
  end;
end;

