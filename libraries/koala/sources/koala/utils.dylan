Module:    internals
Synopsis:  Utilities
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define macro iff
  { iff(?test:expression, ?true:expression, ?false:expression) }
    => { if (?test) ?true else ?false end }
  { iff(?test:expression, ?true:expression) }
    => { if (?test) ?true end }
end;

define macro with-restart
  { with-restart (?condition:expression, #rest ?initargs:*)
      ?:body
    end }
    => { block ()
	   ?body
	 exception (?condition, init-arguments: vector(?initargs))
	   values(#f, #t)
	 end }
end macro with-restart;

define macro with-simple-restart
  { with-simple-restart (?format-string:expression, ?format-args:*)
      ?:body
    end }
    => { with-restart (<simple-restart>,
		       format-string: ?format-string,
		       format-arguments: vector(?format-args))
	   ?body
         end }
end macro with-simple-restart;

define class <sealed-constructor> (<object>) end;
define sealed domain make (subclass(<sealed-constructor>));
define sealed domain initialize (<sealed-constructor>);

define macro inc!
  { inc! (?place:expression, ?dx:expression) }
    => { ?place := ?place + ?dx; }
  { inc! (?place:expression) }
    => { ?place := ?place + 1; }
end macro inc!;

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
    (thing :: <expiring-mixin>) => (expired? :: <boolean>)
  thing.mod-time == #f
  | begin
      let now = current-date();
      (now - thing.mod-time) < thing.duration
    end
end expired?;


define function file-contents
    (filename :: <pathname>) => (contents :: false-or(<string>))
  // In FD 2.0 SP1 if-does-not-exist: #f still signals an error if the file doesn't exist.
  // Remove this block when fixed.  (Reported to Fun-O August 2001.)
  block ()
    with-open-file(input-stream = filename,
                   direction: #"input",
                   if-does-not-exist: #f)
      read-to-end(input-stream)
    end
  exception (<file-does-not-exist-error>)
    #f
  end
end file-contents;


///
/// Simple resource pools
///

// Called to create a new resource when no existing resource is found.
// reinitialize-resource is NOT called on newly created resources.
define generic new-resource
    (resource-class :: <class>, #rest init-args) => (resource :: <object>);

// Called to reset a resource to it's initial, unused state.
define generic reinitialize-resource
    (resource :: <object>);

define method reinitialize-resource
    (resource :: <table>)
  remove-all-keys!(resource);
end;

define generic deallocate-resource
    (resource-class :: <class>, resource :: <object>);

// Implement this if your resource class doesn't support the size generic function.
define generic resource-size
    (resource :: <object>);

// Default method
define method resource-size
    (resource :: <object>)
  size(resource)
end;

// Default method
define method new-resource
    (resource-class :: <class>, #rest args) => (resource :: <object>)
  apply(make, resource-class, args)
end;

//with-resource(table = <string-table>, case-sensitive?: #t) ... end;
define macro with-resource
  { with-resource (?rname:name = ?rtype:expression ?keys-and-vals:*)
      ?:body
    end }
    => { let ?rname :: false-or(?rtype) = #f;
         block ()
           ?rname := allocate-resource(?rtype ?keys-and-vals);
           ?body
         cleanup
           deallocate-resource(?rtype, ?rname);
         end }
end;

define variable *resource-pools* = make(<table>);
define constant $default-pool-max-size :: <integer> = 5;  // ---TODO: make this bigger after done debugging

define class <resource-pool> (<object>)
  slot pool-resources :: <stretchy-vector> = make(<stretchy-vector>);
  slot pool-max-size  :: <integer> = $default-pool-max-size, init-keyword: #"max-size";
end;

define function get-resource-pool
    (class :: <class>, #key max-size :: <integer> = $default-pool-max-size)
 => (pool :: <resource-pool>)
  let pool = element(*resource-pools*, class, default: #f);
  if (pool)
    // ---TODO: Should find the pool with the closest max-size instead.
    pool-max-size(pool) := max-size;
    pool
  else
    *resource-pools*[class] := make(<resource-pool>, max-size: max-size);
  end
end;

define method allocate-resource
    (resource-class :: <class>, #rest init-args, #key size: sz = 20)
 => (resource :: <object>)
  let smallest-diff = $maximum-integer;
  let smallest-index = #f;
  let empty-index = #f;
  let pool = get-resource-pool(resource-class);
  let resources = pool-resources(pool);
  for (item in resources,
       index from 0)
    if(~item)
      iff(~empty-index, empty-index := index);
    else
      let item-size = resource-size(item);
      let diff = item-size - sz;
      if (diff > 0 & diff < smallest-diff)
        smallest-diff := diff;
        smallest-index := index;
      end;
    end;
  end for;
  if (smallest-index)
    // Found an existing resource.
    log-debug("Found an existing %= resource.", resource-class);
    let match = resources[smallest-index];
    resources[smallest-index] := #f;
    reinitialize-resource(match);
    match
  else
    log-debug("No %= resource found.  Allocating a new one.", resource-class);
    let new = apply(new-resource, resource-class, init-args);
    iff(empty-index,
        resources[empty-index] := new,
        iff(size(resources) < pool-max-size(pool),
            add!(resources, new),
            log-warning("%= resource pool full.  Doing non-pooled allocation.", resource-class)));
    new
  end
end allocate-resource;

define method deallocate-resource
    (resource-class :: <class>, resource :: <object>)
  let pool = get-resource-pool(resource-class);
  let resources = pool-resources(pool);
  let rlen = size(resources);
  block (return)
    for (i from 0 below rlen)
      when (~resources[i])
        resources[i] := resource;
        return();
      end;
    end;
    // No empty element found.  Add it back to the pool if there's room,
    // otherwise let it get garbage collected.
    iff(rlen < pool-max-size(pool),
        add!(resources, resource),
        log-warning("Can't return resource to %= pool.  It will be GCed.", resource-class));
  end;
end;

define method reinitialize-resource
    (resource :: <sequence>)
  for (i from 0 below size(resource))
    resource[i] := #f;
  end;
end;

define method new-resource
    (resource-class :: subclass(<sequence-stream>), #rest args) => (resource :: <sequence-stream>)
  apply(make, resource-class, direction:, #"output", args)
end;

define method reinitialize-resource
    (resource :: <sequence-stream>)
  //---TODO: I've sent mail to common-dylan suggesting the addition of a copy?: keyword
  //         argument to stream-contents so that this doesn't have to make an unneeded
  //         copy.  Hopefully there will be a solution in the next release.
  //         If not, re-implement string streams myself.
  stream-contents(resource, clear-contents?: #t);
end;

define method resource-size
    (resource :: <stream>)
  stream-size(resource)
end;


// multiple-value-setq

//define macro mset
//  { mset(?:expression, ?vars:*) } => { do-mset(?expression, ?vars) ?vars end }
//end macro mset;

define macro pset
  { pset (?vars:*) <= ?:expression end }
    => { do-mset(?expression, ?vars) ?vars end }
end macro pset;

define macro do-mset
  { do-mset(?:expression, ?bind-vars) ?sets end }
    => { let (?bind-vars) = ?expression; ?sets }
bind-vars:
  { } => { }
  { ?:name, ... } => { "bind-" ## ?name ## "", ... }
sets:
  { } => { }
  { ?:name, ... } => { ?name := "bind-" ## ?name ## "" ; ... }
end macro do-mset;

define macro ignore-errors
  { ignore-errors(?error:expression, ?body:expression) }
    => { block ()
           ?body
         exception (?error)
           #f
         end }
end;

// Compare to locator-path elements.
//---TODO: portability - This isn't portable.
define method path-element-equal?
    (elem1 :: <object>, elem2 :: <object>) => (equal? :: <boolean>)
  elem1 = elem2
end;

define method path-element-equal?
    (elem1 :: <string>, elem2 :: <string>) => (equal? :: <boolean>)
  string-equal?(elem1, elem2)
end;

define sideways method locator-path
    (locator :: <file-locator>) => (path :: <sequence>)
  locator-path(locator-directory(locator))
end;

define method date-to-stream
    (stream :: <stream>, date :: <date>)
  let (year, month, day, hours, minutes, seconds) = decode-date(date);
  format(stream, "%d-%s%d-%s%d %s%s:%s%d:%s%d",
         year, iff(month < 10, "0", ""), month, iff(day < 10, "0", ""), day,
         iff(hours < 10, "0", ""), hours, iff(minutes < 10, "0", ""), minutes,
         iff(seconds < 10, "0", ""), seconds);
end;


// <session> and <page-context> inherit this.

define class <attributes-mixin> (<object>)
  constant slot attributes :: <table> = make(<table>);
end;

define method reinitialize-resource
    (resource :: <attributes-mixin>)
  remove-all-keys!(resource.attributes);
end;

define method resource-size
    (resource :: <attributes-mixin>)
  size(attributes(resource));
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


