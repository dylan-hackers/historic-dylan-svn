Module:    httpi
Synopsis:  values with attributes
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant <alist> = limited(<simple-object-vector>, of: <pair>);

define constant $empty-alist = make(<alist>, size: 0);

define function rev-as-alist (pairs :: <list>)
  let n = size(pairs);
  let alist = make(<alist>, size: n);
  for (pair :: <pair> in pairs,
       i = n - 1 then i - 1)
    alist[i] := pair
  end;
  alist
end;

// <tagged-element>?
define class <avalue> (<explicit-key-collection>, <sealed-constructor>)
  slot avalue-value :: <object>,
    required-init-keyword: value:;
  constant slot avalue-alist :: <alist>,
    required-init-keyword: alist:;
end;

//define constant $empty-avalue = make(<avalue>, value: #f, alist: #[]);

define inline sealed method size (av :: <avalue>) => (sz :: <integer>)
  size(av.avalue-alist)
end;

define method key-sequence (av :: <avalue>)
 => (keys :: <simple-object-vector>)
  let alist = av.avalue-alist;
  let keys = make(<simple-object-vector>, size: alist.size);
  map-into(keys, head, alist);
  keys
end;

define inline sealed method forward-iteration-protocol (av :: <avalue>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>);
  let alist = av.avalue-alist;
  let limit :: <integer> = alist.size;
  values(0,
         limit,
         method (av, i :: <integer>)
           i + 1
         end,
         method (av, i :: <integer>, limit :: <integer>)
           i == limit
         end,
         method (av :: <avalue>, i :: <integer>)
           av.avalue-alist[i].head
         end,
         method (av :: <avalue>, i :: <integer>)
           av.avalue-alist[i].tail
         end,
         method (value :: <object>, av :: <avalue>, i :: <integer>)
           error("Can't modify %=", av);
         end,
         method (av, i) i end
         )
end forward-iteration-protocol;

define method find-pair (av :: <avalue>, key :: <string>)
  => (pair :: false-or(<pair>))
  let alist = av.avalue-alist;
  let len = alist.size;
  iterate loop (i :: <integer> = 0)
    if (i == len)
      #f
    else
      let pair = alist[i];
      if (string-equal?(key, pair.head))
        pair
      else
        loop(i + 1)
      end;
    end;
  end;
end find-pair;

define sealed method element (av :: <avalue>, key :: <string>, #key default = unsupplied())
  => (value)
  let pair = find-pair(av, key);
  if (pair)
    pair.tail
  elseif (supplied?(default))
    default
  else
    error("%= is not present as an attribute for %=.", key, av);
  end;
end;

/*
define sealed method element-setter (value , av :: <avalue>, key :: <string>)
  => (value)
  error("Can't modify %=", av);
end;
*/


