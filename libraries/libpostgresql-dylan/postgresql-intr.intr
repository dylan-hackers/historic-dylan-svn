module: postgresql

//
//  A Null-string representation
//
define constant $null-string :: <byte-string> = "(null-string)";

define method export-value(cls == <c-string>, value == $null-string)
    => (res :: <c-string>);
  as(<c-string>, 0);
end method export-value;

define macro PQsetdb
  { PQsetdb (?host:expression, ?port:expression, ?opt:expression,
      ?tty:expression, ?dbname:expression) }
      => { PQsetdbLogin(?host, ?port, ?opt, ?tty, ?dbname, $null-string, $null-string) }
end macro;

//
//  Support Notify Behavior
//
define primary sealed class <postgresql-notify> (<object>)
   slot pgn-ptr :: <PGnotify>, init-keyword: pgn-ptr:;
   slot relname :: <byte-string>;
   slot backend-pid :: <integer>;
end class;

define method initialize( ntfy :: <postgresql-notify>, #next next-method, #key, #all-keys) => ();
  next-method();
  ntfy.relname := ntfy.pgn-ptr.pgNotify$relname;
  ntfy.backend-pid := ntfy.pgn-ptr.pgNotify$be-pid;
end method;

define inline function PQnotifies( conn :: <postgresql-connection> )
        => (result :: false-or(<postgresql-notify>))
   let result-value = PQnotifies-internal(conn);
   if (as(<statically-typed-pointer>, result-value) ~= $null-pointer)
       let retval = make(<postgresql-notify>, pgn-ptr: result-value);
       values(retval);
   else
       #f;
   end if;
end function PQnotifies;

define interface
  #include "postgresql/libpq-fe.h",
    import: all, 
    equate: {"char *" => <c-string>,
             "byte *" => <c-string>},
    map: {"char *" => <byte-string>,
          "byte *" => <byte-string>,
          "pgbool" => <boolean>},
    rename: { "byte" => <postgresql-byte>,
              "PGconn" => <postgresql-connection>,
              "PGresult" => <postgresql-result>,
              "FILE" => <fd-stream>,
              "PQnotifies" => PQnotifies-internal},
    exclude: { "PGnotify" };
  pointer "byte **" => <char-pointer-vector>,
    superclasses: {<c-vector>};
end interface;


