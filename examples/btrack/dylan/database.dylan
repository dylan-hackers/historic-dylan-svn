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
           // If this is the first time the system is being used, init the
           // database with an admin user, etc.
           maybe-initialize-database(_conn);
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

define method query-integer
    (connection :: sql$<connection>, query-format-string :: <byte-string>, #rest format-args)
  let rset = apply(query-db, connection, query-format-string, format-args);
  assert(rset.size == 1,
         "Result set for a 'count' query should have only one row.  Query was %=.",
         query-format-string);
  rset[0][0]
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
    let next-id = query-integer(conn, "select next_record_id from tbl_config");
    let next-batch-start = next-id + $record-id-batch-size;
    update-db(conn, "update tbl_config set next_record_id = ?", next-batch-start);
    // Don't set these until update successful.
    *record-id-next-batch-start* := next-batch-start;
    *next-record-id* := next-id;
  end;
end;


define constant $bug-number-lock = make(<lock>);

define function next-bug-number
    () => (bugnum :: <integer>)
  with-lock ($bug-number-lock)
    with-database-connection (conn)
      let bugnum = query-integer(conn, "select next_bug_number from tbl_config");
      update-db(conn, "update tbl_config set next_bug_number = next_bug_number + 1");
      bugnum
    end
  end
end;

define variable *database-initialized?* :: <boolean> = #f;

// Note that this is only called from inside with-database-connection.
//
define function maybe-initialize-database
    (conn) => ()
  if (*database-initialized?*)
    #t
  else
    let accounts = query-integer(conn, "select count(1) from tbl_account");
    if (accounts > 0)
      *database-initialized?* := #t;
    else
      // initialize database
      create-admin-account();
      *database-initialized?* := #t;
    end;
  end;
end;

define function create-admin-account
    ()
  let now = current-date();
  //---TODO: fix some of these init values.
  let admin-account = make(<account>,
                           id: next-record-id(),
                           mod-count: -1,
                           date-entered: now,
                           date-modified: now,
                           name: "admin",
                           status: 'A',              // correct?
                           password: "admin",
                           email-address: "foo@bar", // fix me
                           email-prefs: 0,
                           permissions: 0,
                           role: 'A');               // correct?
  save-record(admin-account);
end;

