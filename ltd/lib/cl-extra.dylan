Module:    CL-internals
Author:    Peter Norvig
Copyright: 1995, 1996 by Harlequin, Inc.

define variable *gensym-counter* = 0;

define method generate-symbol (#key string)
  as(<symbol>, format-to-string("%S%D", string | "G", *gensym-counter*))
end method;

define method subset? (set1 set2 #key test = \==)
  every(curry(member?, set2, test: test), set1)
end method;

define method set-difference (set1 set2 #key test = \==)
  let result = #();
  for(x in set1)
    if (~ member?(x, set2)) push!(x, result) end;
  end;
  result;
end method;

define method constant? (x)
  instance?(x, type-union(<number>, <string>, <character>, <vector>, <array>,
                          one-of(#t, #f, #(), #"pi")))
  | (instance(x, <pair>) & x.head = #"quote")
end method;

define method replace-in-tree (new, old, tree, #key test = \==)
  // The Dylan version of SUBST
  if (tree == old) 
    new;
  elseif (instance?(tree, <pair>))
    pair(replace-in-tree(new, old, tree.head, test: test),
	 replace-in-tree(new, old, tree.head, test: test));
  else 
    old;
  end if;
end method;

define method replace-multiple-in-tree (alist, tree, #key test = \==)
  // The Dylan version of SUBST
  let p = cl-assoc(tree, alist, test: test);
  if (p) 
    p.tail;
  elseif (instance?(tree, <pair>))
    pair(replace-multiple-in-tree(alist, tree.head, test: test),
	 replace-multiple-in-tree(alist, tree.head, test: test));
  else 
    tree;
  end if;
end method;
