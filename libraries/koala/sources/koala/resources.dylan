Module:    utilities
Synopsis:  Simple resource pools
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND



//// Simple resource pools

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
    (resource-class :: <class>, #rest init-args)
 => (resource :: <object>);

/**
 * Called to reset a resource to it's initial, unused state.
 */
define open generic reinitialize-resource
    (resource :: <object>);

/**
 * Called to deallocate a resource.
 */
define open generic deallocate-resource
    (resource-class :: <class>, resource :: <object>);

/**
 * Implement this if your resource class doesn't support the size generic function.
 */
define open generic resource-size
    (resource :: <object>);


//// Default methods for <object>

define method resource-size
    (resource :: <object>)
  size(resource)
end;

define method new-resource
    (resource-class :: <class>, #rest args)
 => (resource :: <object>)
  apply(make, resource-class, args)
end;


//// <table> resources

define method reinitialize-resource
    (resource :: <table>)
  remove-all-keys!(resource);
end;


//// <sequence> resources

define method reinitialize-resource
    (resource :: <sequence>)
  for (i from 0 below size(resource))
    resource[i] := #f;
  end;
end;


//// <sequence-stream> resources

define method reinitialize-resource
    (resource :: <sequence-stream>)
  //---TODO: I've sent mail to common-dylan suggesting the addition of a copy?: keyword
  //         argument to stream-contents so that this doesn't have to make an unneeded
  //         copy.  Hopefully there will be a solution in the next release.
  //         If not, re-implement string streams myself.
  stream-contents(resource, clear-contents?: #t);
end;

define method new-resource
    (resource-class :: subclass(<sequence-stream>), #rest args) => (resource :: <sequence-stream>)
  apply(make, resource-class, direction:, #"output", args)
end;

define method resource-size
    (resource :: <stream>)
  stream-size(resource)
end;


//// Guts

define variable *resource-pools* = make(<table>);
define constant $default-pool-max-size :: <integer> = 5;  // ---TODO: make this bigger after done debugging
define constant $resource-pool-lock = make(<lock>);

define class <resource-pool> (<object>)
  slot pool-resources :: <stretchy-vector> = make(<stretchy-vector>);
  slot maximum-size  :: <integer> = $default-pool-max-size, init-keyword: #"max-size";
  // ---TODO: New implementation, not yet finished.
  constant slot active-resources   :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot inactive-resources :: <stretchy-vector> = make(<stretchy-vector>);
  slot active-count :: <integer> = 0;
  slot inactive-count :: <integer> = 0;
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
    *resource-pools*[class] := make(<resource-pool>, max-size: max-size);
  end
end;

// Find a resource in the inactive pool and atomically move it to the active pool,
// or allocate a new resource and store it in the active pool.
/*
define method activate-resource
    (pool :: <resource-pool>) => (resource :: <object>)
  let actives = pool.active-resources;
  let inactives = pool.inactive-resources;
  with-lock ($resource-pool-lock)
    if (size(inactives) > 0)
      let resource = inactives[ size( inactives ) - 1 ];
      add!(actives, resource);
      inc!(pool.active-count);
      remove!(inactives, resource);
      dec!(pool.inactive-count);
    else
      */
      

define method allocate-resource
    (resource-class :: <class>, #rest init-args, #key size: sz :: <integer> = 20)
 => (resource :: <object>)
  let smallest-diff = $maximum-integer;
  let smallest-index = #f;
  let empty-index = #f;
  let pool = get-resource-pool(resource-class);
  let resources = pool-resources(pool);
  with-lock ($resource-pool-lock)
    for (item in resources,
         index from 0)
      if (~item)
        iff(~empty-index, empty-index := index);
      else
        let item-size :: <integer> = resource-size(item);
        let diff = item-size - sz;
        if (diff >= 0 & diff < smallest-diff)
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
      apply(new-resource, resource-class, init-args)
    end if
  end with-lock;
end allocate-resource;

define method deallocate-resource
    (resource-class :: <class>, resource :: <object>)
  let pool = get-resource-pool(resource-class);
  let resources = pool-resources(pool);
  let rlen = size(resources);
  block (return)
    with-lock ($resource-pool-lock)
      for (i from 0 below rlen)
        when (~resources[i])
          log-debug("Returning resource %= to pool at position %d.", resource, i);
          resources[i] := resource;
          return();
        end;
      end;
      // No empty element found.  Add it back to the pool if there's room,
      // otherwise let it get garbage collected.
      iff(rlen < maximum-size(pool),
          begin
            log-debug("Returning resource %= to pool at END.", resource);
            pool-resources(pool) := add!(resources, resource)
          end,
          log-warning("Can't return resource to %= pool.  It will be GCed.", resource-class));
    end with-lock;
  end block;
end;

define method test-resource-pools
    () => ()
  local method doit (class)
    let pool :: <resource-pool> = get-resource-pool(class);
    let resources = pool-resources(pool);
    format(*standard-output*, "1: resources.size = %d\n", resources.size);
    with-resource (res = class)
      format(*standard-output*, "2: resources.size = %d\n", resources.size);
    end;
    format(*standard-output*, "3: resources.size = %d\n", resources.size);
  end;
  for (i from 1 to 2)
    doit(<string-table>);
    doit(<string-stream>);
  end;
end;


