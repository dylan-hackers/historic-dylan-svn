module: storage
author: Hannes Mehnert <hannes@mehnert.org>

define variable *directory* = "/";

define sideways method process-config-element
    (node :: <xml-element>, name == #"web-framework")
  let cdir = get-attr(node, #"content-directory");
  if (~cdir)
    log-warning("Web Framework - No content-directory specified!");
  else
    *directory* := cdir;
  end;
  log-info("Web framework content directory = %s", *directory*);
  restore-newest(*directory*);
end;

define variable *storage* = make(<table>);

define variable *version* :: <integer> = 0;

define constant $database-lock = make(<lock>);

define method version () => (res :: <integer>)
  *version*;
end;

define open generic storage-type (type) => (res);

define method storage-type (type) => (res)
  <stretchy-vector>;
end;

define method storage (type)
  let res = element(*storage*, type, default: #f);
  unless (res)
    res := make(storage-type(type));
    *storage*[type] := res;
  end;
  res;
end;

define open generic setup (class) => ();

define method setup (class)
 => ();
  format-out(">>> no setup for: %=\n", class);
end;

define macro with-storage
  { with-storage (?:variable = ?type:expression)
      ?body:body
    end }
 =>  { begin
         with-lock($database-lock)
           let ?variable = storage(?type);
           ?body
         end
       end }
end;

define open generic save (object :: <object>) => ();

define method save (object) => ()
  with-lock($database-lock)
    add!(storage(object.object-class), object);
  end;
end;

define class <storage> (<object>)
  constant slot hash-table = *storage*;
  constant slot table-version = *version*;
end;

define constant $filename = last(split(application-name(), '/'));

define method dump-data () => ()
  with-lock ($database-lock)
    let loc = concatenate(*directory*,
                          $filename,
                          "-",
                          integer-to-string(*version*));
    let dood = make(<dood>,
                    locator: loc,
                    direction: #"output",
                    if-exists: #"replace");
    dood-root(dood) := make(<storage>);
    dood-commit(dood);
    dood-close(dood);
    *version* := *version* + 1;
  end;
end;

define method restore (filename :: <string>) => ()
  format-out("restoring %s\n", filename);
  let dood = make(<dood>,
                  locator: filename,
                  direction: #"input");
  let storage = dood-root(dood);
  dood-close(dood);
  with-lock ($database-lock)
    *storage* := storage.hash-table;
    if (storage.table-version >= *version*)
      *version* := storage.table-version + 1;
    else
      *version* := *version* + 1;
    end;
  end;
  for (class in key-sequence(*storage*))
    setup(class);
  end for;
end;

define method restore-newest (directory :: <string>) => ()
  let file = #f;
  let latest-version = 0;
  do-directory(method(directory :: <pathname>,
                      filename :: <string>,
                      type :: <file-type>)
                   if (type == #"file")
                     let version = split-file(filename);
                     if (version > latest-version)
                       latest-version := version;
                       file := filename
                     end;
                   end;
               end, directory);
  if (file)
    restore(concatenate(directory, "/", file));
  end;
end;

define function dumper (#key interval :: <integer> = 300, do-something :: false-or(<function>) = #f) => ()
  make(<thread>,
       function: method()
                     sleep(23);
                     while(#t)
                       dump-data();
                       if (do-something)
                         do-something()
                       end;
                       sleep(interval);
                     end;
                 end);
end;

define function split-file (filename :: <string>) => (version :: <integer>)
  let elements = split(filename, '-');
  if ((elements.size = 2) & (elements[0] = $filename))
    string-to-integer(elements[1]);
  else
    0
  end;
end;
