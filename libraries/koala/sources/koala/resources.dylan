Module:    utilities
Synopsis:  Simple resource pools
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND



//// Simple resource pools

// ---TODO: Look at DUIM's protocols and maybe implement the resource
//          protocol that way.  Currently there's nothing in the source
//          to make it explicit that something is following the resource
//          protocol.

// A resource class must implement the following methods:
// - new-resource(resource-class, #rest init-args) => resource
//   A default method is provided that simply calls apply(make, resource-class, init-args)
// - reinitialize-resource(resource, #rest init-args)
//   No default method is provided.
// - resource-deallocated(resource)
//   A default method is provided that does nothing.  You could use this to deallocate
//   sub-resources, for example.
// - resource-size(resource) => (size :: <integer>)
//   A default method is provided that calls size(resource).

/**
 * The most common way to safely use a resource.
 * Example: with-resource(table = <string-table>, case-sensitive?: #t) ... end;
 */
define macro with-resource
  { with-resource (?res:name = ?rtype:expression ?keys-and-vals:*)
      ?:body
    end }
    => { let ?res :: false-or(?rtype) = #f;
         block ()
           ?res := allocate-resource(?rtype ?keys-and-vals);
           ?body
         cleanup
           when (?res)
             deallocate-resource(?rtype, ?res);
           end;
         end }
end;

/**
 * Called to create a new resource when no existing resource is found.
 * reinitialize-resource is NOT called on newly created resources.
 */
define open generic new-resource
    (resource-class :: <class>, #rest init-args, #key)
 => (resource :: <object>);

/**
 * Called to reset a resource to it's initial, unused state.
 */
define open generic reinitialize-resource
    (resource :: <object>, #rest init-args, #key);

/**
 * Called when a resource becomes unused.
 */
define open generic resource-deallocated
    (resource :: <object>);

/**
 * Implement this if your resource class doesn't support the size generic function.
 */
define open generic resource-size
    (resource :: <object>)
 => (size :: <integer>);


//// Default methods for <object>

define method resource-size
    (resource :: <object>) => (size :: <integer>)
  size(resource)
end;

define method new-resource
    (resource-class :: <class>, #rest args, #key)
 => (resource :: <object>)
  apply(make, resource-class, args)
end;

define open method resource-deallocated
    (resource :: <object>)
  // do nothing
end;


//// <table> resources

define method reinitialize-resource
    (resource :: <table>, #rest init-args, #key)
  remove-all-keys!(resource);
end;


//// <sequence> resources

define method reinitialize-resource
    (resource :: <sequence>, #rest init-args, #key)
  for (i from 0 below size(resource))
    resource[i] := #f;
  end;
end;


//// <sequence-stream> resources

define method reinitialize-resource
    (resource :: <sequence-stream>, #rest init-args, #key)
  //---TODO: I've sent mail to common-dylan suggesting the addition of a copy?: keyword
  //         argument to stream-contents so that this doesn't have to make an unneeded
  //         copy.  Hopefully there will be a solution in the next release.
  //         If not, re-implement string streams myself.
  stream-contents(resource, clear-contents?: #t);
end;

define method new-resource
    (resource-class :: subclass(<sequence-stream>), #rest args, #key)
 => (resource :: <sequence-stream>)
  apply(make, resource-class, direction:, #"output", args)
end;

define method resource-size
    (resource :: <stream>) => (size :: <integer>)
  stream-size(resource)
end;


//// Guts

define variable *resource-pools* = make(<table>);
define constant $default-pool-max-size :: <integer> = 5;  // ---TODO: make this bigger after done debugging

define class <resource-pool> (<object>)
  slot maximum-size  :: <integer> = $default-pool-max-size, init-keyword: #"max-size";
  // We don't really need to keep track of active resources, but we do for debugging purposes.
  constant slot active-resources   :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot inactive-resources :: <stretchy-vector> = make(<stretchy-vector>);
  slot active-count   :: <integer> = 0;
  slot inactive-count :: <integer> = 0;
  constant slot resource-lock :: <lock> = make(<lock>);
  slot %resource-notification :: <notification>;
  // for debugging
  constant slot resource-class :: <class>, required-init-keyword: #"class";
end;

define method resource-notification
    (pool :: <resource-pool>) => (notif :: <notification>)
  if (slot-initialized?(pool, %resource-notification))
    %resource-notification(pool)
  else
    %resource-notification(pool) := make(<notification>, lock: resource-lock(pool))
  end
end;

define function get-resource-pool
    (class :: <class>, #key max-size :: <integer> = $default-pool-max-size)
 => (pool :: <resource-pool>)
  let pool = element(*resource-pools*, class, default: #f);
  if (pool)
    // ---TODO: Should find the pool with the closest max-size instead.
    maximum-size(pool) := max-size;
    pool
  else
    *resource-pools*[class] := make(<resource-pool>, class: class, max-size: max-size);
  end
end;

/*
Resource allocation algorithm:
  acquire resource lock
    loop
      if there are inactive resources
         and one is bigger than requested size
      then
        reinitialize the existing resource
        move it to active set
        return it
      elseif total resources in pool are < pool max resources
        create a new resource
        store it in active set
        return it
      else
        allocate the resource unpooled and let the GC deal with it
      end if
    end loop
  release resource lock
*/

define function allocate-resource
    (resource-class :: <class>,
     #rest init-args,
     #key size: sz :: <integer> = 20,
     #all-keys)
 => (resource :: <object>)
  let pool :: <resource-pool> = get-resource-pool(resource-class);
  block (return)
    with-lock (pool.resource-lock)
      let pool-full? = (pool.inactive-count + pool.active-count) >= pool.maximum-size;
      let resource = when (pool.inactive-count > 0)
                       find-inactive-resource(pool, sz, pool-full?, init-args)
                     end;
      if (resource)
        return(resource);
      elseif (~pool-full?)
        let resource = apply(new-resource, resource-class, init-args);
        add!(pool.active-resources, resource);
        inc!(pool.active-count);
        return (resource);
      else
        log-debug("Resource full.  Allocating non-pooled instance. %=",
                  resource-class);
        apply(new-resource, resource-class, init-args)
      end if
    end with-lock
  end block
end allocate-resource;

// Find the resource closest in size to sz, but > sz.
// This is called with the lock for the associated resource pool held.
//
define inline method find-inactive-resource
    (pool :: <resource-pool>, sz :: <integer>, pool-full? :: <boolean>, init-args)
 => (resource :: <object>)                      // may return #f
  let inactives :: <stretchy-vector> = pool.inactive-resources;
  let min-diff  :: <integer> = $maximum-integer;
  let min-index :: <integer> = -1;
  // This could get really expensive if there are a lot of inactive resources.
  // Might want to have a way to short-circuit the loop if a reasonable match is found.
  for (item in inactives,
       index from 0)
    let diff :: <integer> = resource-size(item) - sz;
    if (diff >= 0 & diff < min-diff)
      // always prefer a resource that's bigger than the requested size
      min-diff := diff;
      min-index := index;
    elseif (pool-full? & abs(diff) < min-diff)
      // but if the pool is full, allocate whatever resource has the closest size
      min-diff := abs(diff);
      min-index := index
    end;
  end for;
  when (min-index >= 0)
    let resource = inactives[min-index];
    remove!(inactives, resource);
    add!(pool.active-resources, resource);
    dec!(pool.inactive-count);
    inc!(pool.active-count);
    apply(reinitialize-resource, resource, init-args);
    resource
  end
end find-inactive-resource;

define function deallocate-resource
    (resource-class :: <class>, resource :: <object>)
  let pool :: <resource-pool> = get-resource-pool(resource-class);
  with-lock (pool.resource-lock)
    remove!(pool.active-resources, resource);
    dec!(pool.active-count);
    if ((pool.active-count + pool.inactive-count) < pool.maximum-size)
      add!(pool.inactive-resources, resource);
      inc!(pool.inactive-count);
    else
      log-warning("Can't return resource %= to pool.  Hopefully it will be GCed.", resource);
    end;
    resource-deallocated(resource);
  end with-lock;
end;

define method describe-pool
    (pool :: <resource-pool>)
  log-debug("active: %d,%d, inactive: %d,%d - %s",
            pool.active-resources.size, pool.active-count,
            pool.inactive-resources.size, pool.inactive-count,
            pool.resource-class);
end;

define method test-resource
    (class) => ()
  local method doit (class)
    let pool :: <resource-pool> = get-resource-pool(class);
    describe-pool(pool);
    with-resource (res = class)
      describe-pool(pool);
      with-resource(res = class)
        describe-pool(pool)
      end;
    end;
    describe-pool(pool);
  end;
  log-debug("*** Testing resource pools");
  for (i from 1 to 6)
    log-debug("");  // blank line
    doit(class);
  end;
end;


