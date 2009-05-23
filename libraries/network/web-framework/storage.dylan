module: storage
author: Hannes Mehnert <hannes@mehnert.org>

define variable *content-directory* = false-or(<directory-locator>) = #f;

define constant $database-lock = make(<read-write-lock>);

define sideways method process-config-element
    (server :: <http-server>, node :: <xml-element>, name == #"web-framework")
  let cdir = get-attr(node, #"content-directory");
  if (~cdir)
    log-warning("Web Framework - No content-directory specified!");
  else
    *content-directory*
      := merge-locators(as(<directory-locator>, cdir), server.server-root);
  end;
  log-info("Web framework content directory = %s",
           as(<string>, *content-directory*));
  restore-newest(*content-directory*);
end;

define macro getter-and-setter
 { getter-and-setter(?:name; constant ?args:* slot ?slot-name:name :: ?slot-type:expression ?rest:*; ?slots:*) }
    => { define method ?slot-name (object :: ?name) => (res :: ?slot-type)
           //format-out("accessing constant slot of %s\n", object.object-class);
           let res = #f;
           wait-for($database-lock, mode: #"read");
           res := "%" ## ?slot-name(object);
           release($database-lock);
           res;
         end;
         getter-and-setter(?name; ?slots);
        }
 { getter-and-setter(?:name; ?args:* slot ?slot-name:name :: ?slot-type:expression ?rest:*; ?slots:*) }
    => { define method ?slot-name (object :: ?name) => (res :: ?slot-type)
           //format-out("reading slot of %s\n", object.object-class);
           let res = #f;
           wait-for($database-lock, mode: #"read");
           res := "%" ## ?slot-name(object);
           release($database-lock);
           res;
         end;
         define method ?slot-name ## "-setter" (new-val :: ?slot-type, object :: ?name) => (res :: ?slot-type)
             //format-out("writing slot of %s\n", object.object-class);
             wait-for($database-lock, mode: #"write");
             "%" ## ?slot-name ## "-setter"(new-val, object);
             release($database-lock);
             new-val;
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
    { ?args:* slot ?:name :: ?slot-type:expression ?rest:* }
    => { ?args slot "%" ## ?name :: ?slot-type ?rest }
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
         wait-for($database-lock, mode: #"read");
         let ?variable = storage(?type);
         ?body;
         release($database-lock);
       end }
   { with-storage ()
       ?body:body
     end }
 => { begin
         wait-for ($database-lock, mode: #"read");
         ?body;
         release($database-lock);
      end }
end;

define open generic save (object :: <object>) => ();

define method save (object) => ()
  add-object(storage(object.object-class), object);
end;

define method add-object (list :: <collection>, ele :: <object>)
  wait-for($database-lock, mode: #"write");
  *storage*[ele.object-class] := add!(list, ele);
  release($database-lock);
end;

define method add-object (table :: <table>, ele :: <object>)
  let key = ele.key;
  wait-for($database-lock, mode: #"write");
  table[key] := ele;
  release($database-lock);
end;

define class <storage> (<object>)
  constant slot hash-table = *storage*;
  constant slot table-version = *version*;
end;

define constant $filename = last(split(application-name(), file-system-separator()));

define inline function generate-filename
    () => (res :: <file-locator>)
  merge-locators(as(<file-locator>,
                      concatenate($filename, ".", integer-to-string(*version*))),
                 *content-directory*)
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
  wait-for ($database-lock, mode: #"write");
  really-dump-all-data();
  release($database-lock);
end;

define method restore (directory :: <pathname>, filename :: <string>) => ()
  let loc = merge-locators(as(<file-locator>, filename),
                           as(<directory-locator>, directory));
  let dood = make(<dood>,
                  locator: loc,
                  direction: #"input");
  let storage-root = dood-root(dood);
  dood-close(dood);
  wait-for ($database-lock, mode: #"write");
  *storage* := storage-root.hash-table;
  *version* := storage-root.table-version;
  release($database-lock);
  format-out("Restored %s\n", as(<string>, loc));
  for (class in key-sequence(*storage*))
    setup(class);
  end for;
end;

define method restore-newest (directory :: <pathname>) => ()
  let file = #f;
  let latest-version = 0;
  local method find-latest (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file")
            let version = split-file(filename);
            if (version > latest-version)
              latest-version := version;
              file := filename
            end;
          end;
        end;
  do-directory(find-latest, directory);
  if (file)
    restore(directory, file);
  end;
end;

define constant $dump? = make(<lock>);
define constant $dump-notification
  = make(<notification>, lock: $dump?);

define function query-dump ()
  with-lock($dump?)
    release($dump-notification);
  end;
end;

define function do-dump ()
  make(<thread>,
       function: method()
                     while(#t)
                       with-lock($dump?)
                         wait-for($dump-notification)
                       end;
                       dump-data();
                       sleep(10); //maximal dump every 10 seconds
                     end
                 end);
end;

define function dumper
    (#key interval :: <integer> = 300, do-something :: false-or(<function>) = #f)
 => ()
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
