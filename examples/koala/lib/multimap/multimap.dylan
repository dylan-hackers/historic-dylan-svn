module:    multimap
synopsis:  Implementation of multimap described in proposal, 19 March 2002.
           When accepted the contents of this module will be merged into
           the table-extensions library
author:    Douglas M. Auclair
copyright: (c) 2002, under the copyright of the GwydionDylan Maintainers

// Modified to work in Functional Developer by using containment rather than
// inheritance, since FD seals element on <object-table>.  --Carl Gay 24-Apr-2002

// see the proposal write-up in this directory.

define class <multimap> (<object>)
  constant slot object-table :: <object-table> = make(<object-table>);
end;

define constant $not-supplied = #"default-not-supplied";

define method mm-element
    (mm :: <multimap>, key, #key default = $not-supplied)
 => (seq :: <sequence>)
  let ans = element(mm.object-table, key, default: default);
  if(ans == $not-supplied) 
    error("The key %= is not in this <multimap>", key);
  end if;
  if(instance?(ans, <sequence>))
    as(<list>, ans);
  else 
    list(ans);
  end if;
end method mm-element;

define method mm-element-setter
    (elt, mm :: <multimap>, key)
 => (seq :: <sequence>)
  let result = pair(elt, element(mm.object-table, key, default: #()));
  mm.object-table[key] := result
end method mm-element-setter;

