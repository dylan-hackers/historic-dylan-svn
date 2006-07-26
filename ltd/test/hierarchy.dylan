//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Hierarchy.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp
  export includes, unincludes, includees, decludes, included-active-theory-names, show-theory-dag, all-theories;
end module dtp;

// ----------------------------------------------------------------------------
// List of INCLUDE-MAP structures
define variable *include-maps* = #f;

// ----------------------------------------------------------------------------
define class <include-map> (<object>)
  slot include-map-theory-name, init-keyword: #"include-map-theory-name";
  slot include-map-included-names,
       init-keyword: #"include-map-included-names";
end class <include-map>;

// ----------------------------------------------------------------------------
define method include-map-print-function (structure, stream, depth)
  format(stream, "<Theory %S includes %S>", structure.include-map-theory-name,
         structure.include-map-included-names);
end method include-map-print-function;

// ----------------------------------------------------------------------------
define method reset-hierarchy ()
  *include-maps* := #f;
end method reset-hierarchy;

// ----------------------------------------------------------------------------
define method get-im-structure (theory-name)
  cl-find(theory-name, *include-maps*, key: include-map-theory-name);
end method get-im-structure;

// ----------------------------------------------------------------------------
define method includes (theory-name-1, theory-name-2)
  let map = get-im-structure(theory-name-1);
  if (map)
    add-to-end-if-new(theory-name-2, map.include-map-included-names);
  else
    add-to-end(map
                := make-include-map(theory-name: theory-name-1,
                                    included-names: list(theory-name-2)),
               *include-maps*);
  end if;
  map.include-map-included-names;
end method includes;

// ----------------------------------------------------------------------------
define method unincludes (theory-name-1, theory-name-2)
  let map = get-im-structure(theory-name-1);
  if (map)
    map.include-map-included-names
     := remove(map.include-map-included-names, theory-name-2);
    if (~ map.include-map-included-names)
      *include-maps* := remove(*include-maps*, map);
    end if;
    #"t";
  end if;
end method unincludes;

// ----------------------------------------------------------------------------
define method includees (theory-name)
  let map = get-im-structure(theory-name);
  if (map) map.include-map-included-names; end if;
end method includees;

// ----------------------------------------------------------------------------
define method decludes (theory-name)
  let list92543 = includees(theory-name);
  begin
    do(method (x) unincludes(theory-name, x); end method, list92543);
    list92543;
  end;
end method decludes;

// ----------------------------------------------------------------------------
define method included-active-theory-names (theory-name)
  let all-names = make(<deque>);
  block (return)
    for (remaining-names = list(theory-name) then tail(remaining-names),
         name = first(remaining-names) then first(remaining-names),
         until empty?(remaining-names))
      if (~ cl-find(name, all-names))
        push-last(all-names, name);
        remaining-names := concatenate(remaining-names, includees(name));
      end if;
    finally
      return(all-names);
      all-names;
    end for;
  end block;
end method included-active-theory-names;

// ----------------------------------------------------------------------------
define method all-include-theories ()
  union(map(include-map-theory-name, *include-maps*),
        reduce1(concatenate,
                map(include-map-included-names, *include-maps*)));
end method all-include-theories;

// ----------------------------------------------------------------------------
define method show-theory-dag ()
  let children = #f;
  let roots = #f;
  children := map(include-map-included-names, *include-maps*);
  if (children)
    children
     := reduce1(// LTD: Can't convert complex function UNION.
                union, children);
  end if;
  roots
   := choose(complement(method (x) cl-find(x, children); end method),
             all-theories());
  if (~ roots) roots := all-theories(); end if;
  for (root in roots) show-theory-dag-internal(root, 0, #f); end for;
  values();
end method show-theory-dag;

define method show-theory-dag-internal (name, depth, already-seen)
  tab-to(depth);
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               push-char-mode(xp, #"cap1");
               fluid-bind (*print-escape* = #f)
                 write+(pop!(args), xp);
               end fluid-bind;
               pop-char-mode(xp);
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(#t, name);
  if (name == *theory*)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 pprint-tab+(line: 20, 1, xp);
                 write-string++("[Active]", xp, 0, 8);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t);
  end if;
  format-out("\n");
  let children = includees(name);
  let new-seen = union(children, already-seen);
  let new-depth = depth + 1;
  for (child in children)
    show-theory-dag-internal(child, new-depth, new-seen);
  end for;
end method show-theory-dag-internal;

define method tab-to (column)
  for (col from 0 below 3 * column) format-out(" "); end for;
end method tab-to;

// ----------------------------------------------------------------------------
define method all-theories ()
  let theories = #f;
  theories := all-kb-theories();
  theories := concatenate(theories, all-include-theories());
  if (// LTD: Function BOUNDP not yet implemented.
      boundp(#"*proof*"))
    theories := pair(proof-theory(*proof*), theories);
  end if;
  cl-remove-duplicates(theories);
end method all-theories;

// ----------------------------------------------------------------------------
"eof";

