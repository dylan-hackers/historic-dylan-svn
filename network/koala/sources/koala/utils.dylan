Module:    utilities
Synopsis:  Various small utilities
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define class <sealed-constructor> (<object>) end;
define sealed domain make (subclass(<sealed-constructor>));
define sealed domain initialize (<sealed-constructor>);


define macro wrapping-inc!
  { wrapping-inc! (?place:expression) }
    => { let n :: <integer> = ?place;
	 ?place := if (n == $maximum-integer) 0 else n + 1 end; }
end;


// Things that expire.

define class <expiring-mixin> (<object>)
  constant slot duration :: <day/time-duration>
    = encode-day/time-duration(0, 1, 0, 0, 0),      // 1 hour
    init-keyword: #"duration";
  // When the object was last modified (e.g., loaded from a file).
  slot mod-time :: false-or(<date>) = #f;
end;

define method expired?
    (thing :: <expiring-mixin>)
 => (expired? :: <boolean>);
  thing.mod-time == #f | begin
      let now = current-date();
      (now - thing.mod-time) < thing.duration
    end;
end method expired?;



define function file-contents
    (filename :: <pathname>, #key error? :: <boolean>)
 => (contents :: false-or(<string>))
  // In FD 2.0 SP1 if-does-not-exist: #f still signals an error if the file doesn't exist.
  // Remove this block when fixed.  (Reported to Fun-O August 2001.)
  block ()
    with-open-file(input-stream = filename,
                   direction: #"input",
                   if-does-not-exist: if (error?) #"error" else #f end)
      read-to-end(input-stream)
    end
  exception (ex :: <file-does-not-exist-error>)
    if (error?)
      signal(ex)
    else
      #f
    end
  end
end file-contents;



//// multiple-value-setq

//define macro mset
//  { mset(?:expression, ?vars:*) } => { do-mset(?expression, ?vars) ?vars end }
//end macro mset;

define macro pset
  { pset (?vars:*) ?:expression end }
    => { do-mset(?expression, ?vars) ?vars end }
end;

define macro do-mset
  { do-mset(?:expression, ?bind-vars) ?sets end }
    => { let (?bind-vars) = ?expression; ?sets }
bind-vars:
  { } => { }
  { ?:name, ... } => { "bind-" ## ?name ## "", ... }
sets:
  { } => { }
  { ?:name, ... } => { ?name := "bind-" ## ?name ## "" ; ... }
end;



define sideways method locator-path
    (locator :: <file-locator>) => (path :: <sequence>)
  locator-path(locator-directory(locator))
end;



define method parent-directory
    (dir :: <locator>, #key levels = 1) => (dir :: <directory-locator>)
  for (i from 1 to levels)
    // is there a better way to get the containing directory?
    dir := simplify-locator(subdirectory-locator(dir, ".."));
  end;
  dir
end;


//// Attributes

define class <attributes-mixin> (<object>)
  constant slot attributes :: <table> = make(<table>);
end;

define generic get-attribute (this :: <attributes-mixin>, key :: <object>, #key);
define generic set-attribute (this :: <attributes-mixin>, key :: <object>, value :: <object>);
define generic remove-attribute (this :: <attributes-mixin>, key :: <object>);

// API
define method get-attribute
    (this :: <attributes-mixin>, key :: <object>, #key default)
 => (attribute :: <object>)
  element(this.attributes, key, default: default)
end;

// API
define method set-attribute
    (this :: <attributes-mixin>, key :: <object>, value :: <object>)
  this.attributes[key] := value;
end;

// API
define method remove-attribute
    (this :: <attributes-mixin>, key :: <object>)
  this.attributes[key] := #f;
end;



//// XML/HTML

define table $html-quote-map
  = { '<' => "&lt;",
      '>' => "&gt;",
      '&' => "&amp;",
      '"' => "&quot;"
      };

// I'm sure this could use a lot of optimization.
define function quote-html
    (text :: <string>, #key stream)
  if (~stream)
    with-output-to-string (s)
      quote-html(text, stream: s)
    end
  else
    for (char in text)
      let translation = element($html-quote-map, char, default: char);
      iff(instance?(translation, <sequence>),
          write(stream, translation),
          write-element(stream, translation));
    end;
  end;
end quote-html;



// Abuncha functions that get called by init-server.

define variable *init-functions* = make(<stretchy-vector>);

define function register-init-function (f)
  *init-functions* := add!(*init-functions*, f);
end;

define function run-init-functions ()
  do(method (f) f() end, *init-functions*);
end;


//// Tries who's keys are strings

define class <string-trie> (<object>)
  constant slot trie-children :: <string-table> = make(<string-table>);
  slot trie-object :: <object>,
    required-init-keyword: object:;
end;

define class <trie-error> (<format-string-condition>, <error>)
end;

define method add-object
    (trie :: <string-trie>, path :: <sequence>, object :: <object>,
     #key replace?)
 => ();
  local method real-add (trie, rest-path)
          if (rest-path.size = 0)
            if (trie.trie-object = #f | replace?)
              trie.trie-object := object;
            else
              signal(make(<trie-error>, 
			   format-string: "Trie already contains an object for the "
			                  "given path (%=).",
			   format-arguments: list(path)));
            end if;
          else
            let first-path = rest-path[0];
            let other-path = copy-sequence(rest-path, start: 1);
            let children = trie-children(trie);
            let child = element(children, first-path, default: #f);
            unless (child)
              let node = make(<string-trie>, object: #f);
              children[first-path] := node;
              child := node;
            end;
            real-add(child, other-path)
          end;
        end method real-add;
  real-add(trie, path)
end method add-object;

define method remove-object
    (trie :: <string-trie>, path :: <sequence>)
 => ();
  let nodes = #[];
  let node = reduce(method (a, b)
      nodes := add!(nodes, a);
      a.trie-children[b]
    end, trie, path);
  let object = node.trie-object;
  node.trie-object := #f;
  block (stop)
    for (node in reverse(nodes), child in reverse(path))
      if (size(node.trie-children[child].trie-children) = 0 & ~node.trie-object)
        remove-key!(node.trie-children, child);
      else
        stop()
      end if;
    end for;
  end;
  object;
end;


// Find the object with the longest path, if any.
// 2nd return value is the part of the path that
// came after where the object matched.

define method find-object
    (trie :: <string-trie>, path :: <sequence>)
 => (object :: <object>, rest-path :: <sequence>);
  local method real-find (trie, path, object, rest)
          if (empty?(path))
            values(object, rest)
          else
            let child = element(trie.trie-children, head(path), default: #f);
            if (child)
              real-find(child, tail(path), child.trie-object | object,
                  if (child.trie-object)
                    tail(path)
                  else
                    rest
                  end if);
            else
              values(object, rest);
            end
          end
        end method real-find;
  real-find(trie, as(<list>, path), trie.trie-object, #());
end method find-object;

