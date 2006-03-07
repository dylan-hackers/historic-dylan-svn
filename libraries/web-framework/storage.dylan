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

define method version () => (res :: <integer>)
  *version*;
end;

define method storage (type) => (res :: <stretchy-vector>)
  let res = element(*storage*, type, default: #f);
  unless (res)
    res := make(<stretchy-vector>);
    *storage*[type] := res;
  end;
  res;
end;

define method save (object) => ()
  add!(storage(object.object-class), object);
end;

define class <storage> (<object>)
  constant slot hash-table = *storage*;
  constant slot table-version = *version*;
end;

define method dump-data () => ()
  let dood = make(<dood>,
                  locator: concatenate(application-name(), "-", integer-to-string(*version*)),
                  direction: #"output",
                  if-exists: #"replace");
  dood-root(dood) := make(<storage>);
  dood-commit(dood);
  dood-close(dood);
  *version* := *version* + 1;
end;

define method restore (filename :: <string>) => ()
  let dood = make(<dood>,
                  locator: filename,
                  direction: #"input");
  let storage = dood-root(dood);
  dood-close(dood);
  *storage* := storage.hash-table;
  if (storage.table-version >= *version*)
    *version* := storage.table-version + 1;
  else
    *version* := *version* + 1;
  end;
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
    restore(file);
  end;
end;

define function dumper (#key interval :: <integer> = 300) => ()
  make(<thread>,
       function: method()
                     sleep(23);
                     while(#t)
                       dump-data();
                       sleep(interval);
                     end;
                 end);
end;

define function split-file (filename :: <string>) => (version :: <integer>)
  let elements = split(filename, '-');
  if ((elements.size = 2) & (elements[0] = application-name()))
    string-to-integer(elements[1]);
  else
    0
  end;
end;
