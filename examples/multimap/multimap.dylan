module:    multimap
synopsis:  Implementation of multimap described in proposal, 19 March 2002.
           When accepted the contents of this module will be merged into
           the table-extensions library
author:    Douglas M. Auclair
copyright: (c) 2002, under the copyright of the GwydionDylan Maintainers

// see the proposal write-up in this directory.

define class <multimap> (<object-table>) end;
define constant $not-supplied = #"default-not-supplied";
define method element(mm :: <multimap>, key, #key default = $not-supplied)
 => (seq :: <sequence>)
  let ans = next-method();
  if(ans == $not-supplied) 
    error("The key %= is not in this <multimap>", key);
  end if;
  if(instance?(ans, <sequence>))
    as(<list>, ans);
  else 
    list(ans);
  end if;
end method element;

define method element-setter(elt, mm :: <multimap>, key, #next next)
 => (seq :: <sequence>)
  let result = pair(elt, element(mm, key, default: #()));
  next(result, mm, key);
  result;
end method element-setter;

