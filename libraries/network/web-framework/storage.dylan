module: storage
author: Hannes Mehnert <hannes@mehnert.org>

define variable *directory* = "/";

define constant $database-lock = make(<recursive-lock>);

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

define macro getter-and-setter
 { getter-and-setter(?:name; constant ?args:* slot ?slot-name:name :: ?slot-type:expression ?rest:*; ?slots:*) }
    => { define method ?slot-name (object :: ?name) => (res :: ?slot-type)
           //format-out("accessing constant slot of %s\n", object.object-class);
           with-lock($database-lock)
             "%" ## ?slot-name(object);
           end;
         end;
         getter-and-setter(?name; ?slots);
        }
 { getter-and-setter(?:name; ?args:* slot ?slot-name:name :: ?slot-type:expression ?rest:*; ?slots:*) }
    => { define method ?slot-name (object :: ?name) => (res :: ?slot-type)
           //format-out("reading slot of %s\n", object.object-class);
           with-lock($database-lock)
             "%" ## ?slot-name(object);
           end;
         end;
         define method ?slot-name ## "-setter" (new-val :: ?slot-type, object :: ?name) => (res :: ?slot-type)
             //format-out("writing slot of %s\n", object.object-class);

             with-lock($database-lock)
               "%" ## ?slot-name ## "-setter"(new-val, object);
             end;
         end;
         getter-and-setter(?name; ?slots);
       }
    { getter-and-setter(?:name) } => { }
end;


define macro storage-class-definer
 { define storage-class ?:name (?superclasses:expression)
     ?slots:*
   end } =>
  { define-class(?name; ?superclasses; ?slots);
    getter-and-setter(?name; ?slots) }
end;

define macro define-class
{ define-class(?:name; ?superclasses:expression; ?slots:*) }
    => { define class ?name (?superclasses)
           ?slots;
         end }
        
  slots:
    { } => { }
    { ?slot:* ; ... } => { ?slot ; ... }

  slot:
    { } => { }
    { ?args:* slot ?:name :: ?slot-type:expression ?rest:* } => { ?args slot "%" ## ?name :: ?slot-type ?rest }
end;

define variable *storage* = make(<table>);

define variable *version* :: <integer> = 0;

define variable *rev* :: <integer> = 0;

define open generic storage-type (type) => (res);

define open generic key (class) => (res);
define method key (class) => (res)
  #f;
end;
define method storage-type (type) => (res)
  <stretchy-vector>;
end;

define open generic storage (type) => (res);
define method storage (type) => (res)
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
   { with-storage ()
       ?body:body
     end }
 => { begin
         with-lock($database-lock)
           ?body;
         end;
      end }
end;

define open generic save (object :: <object>) => ();

define method save (object) => ()
  with-lock($database-lock)
    add-object(storage(object.object-class), object);
  end;
end;

define method add-object (list :: <collection>, ele :: <object>)
  *storage*[ele.object-class] := add!(list, ele);
end;

define method add-object (table :: <table>, ele :: <object>)
  table[ele.key] := ele
end;

define class <storage> (<object>)
  constant slot hash-table = *storage*;
  constant slot table-version = *version*;
end;

define constant $filename = last(split(application-name(), '/'));

define inline function generate-filename () => (res :: <string>)
  concatenate(*directory*, $filename, ".", integer-to-string(*version*));
end;

define function really-dump-all-data () => ()
  *version* := *version* + 1;
  let loc = generate-filename();
  let dood = make(<dood>, locator: loc, direction: #"output", if-exists: #"replace");
  dood-root(dood) := make(<storage>);
  dood-commit(dood);
  dood-close(dood);
  *rev* := 0;
end;

define method dump-data () => ()
  with-lock ($database-lock)
    really-dump-all-data();
  end;
end;

define method restore (directory :: <string>, filename :: <string>) => ()
  let dood = make(<dood>,
                  locator: merge-locators(as(<file-locator>, filename),
                                          as(<directory-locator>, directory)),
                  direction: #"input");
  let storage-root = dood-root(dood);
  dood-close(dood);
  with-lock ($database-lock)
    *storage* := storage-root.hash-table;
    *version* := storage-root.table-version;
  end;
  format-out("Restored %= %=\n", directory, filename);
  for (class in key-sequence(*storage*))
    setup(class);
  end for;
end;

define method restore-newest (directory :: <string>) => ()
  let file = #f;
  let latest-version = 0;
  do-directory(method (dir :: <pathname>, filename :: <string>, type :: <file-type>)
                 if (type == #"file")
                   let version = split-file(filename);
                   if (version > latest-version)
                     latest-version := version;
                     file := filename
                   end;
                 end;
               end, directory);
  if (file)
    restore(directory, file);
  end;
end;

define variable *dump?* = #f;

define function query-dump ()
  *dump?* := #t;
end;

define function do-dump ()
  make(<thread>,
       function: method()
                     sleep(4);
                     let i :: <integer> = 0;
                     while(#t)
                       if (i > 60)
                         *dump?* := #t;
                         i := 0;
                       end;
                       if (*dump?*)
                         dump-data();
                         *dump?* := #f;
                       end;
                       i := i + 1;
                       sleep(60);
                     end
                 end);
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
  let elements = split(filename, '.');
  block()
    if ((elements.size = 2) & (elements[0] = $filename))
      string-to-integer(elements[1]);
    else
      0;
    end;
  exception (e :: <error>)
    0
  end;
end;
