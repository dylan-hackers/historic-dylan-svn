module: postgresql

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
              "PGnotify" => <postgresql-notify>,
              "FILE" => <fd-stream>};
  pointer "byte **" => <char-pointer-vector>,
    superclasses: {<c-vector>};
end interface;


