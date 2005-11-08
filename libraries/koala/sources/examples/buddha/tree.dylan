module: tree
author: Hannes Mehnert <hannes@mehnert.org>

define class <node> (<object>)
  constant slot value :: <object>, init-keyword: value:;
  slot children :: <list> = #(), init-keyword: children:;
end;

define method add-node (parent :: <node>, node :: <node>)
  //hmm, maybe add as last element, not first?
  parent.children := add!(parent.children, node);
end;

define method traverse (node :: <node>) => (list :: <list>)
  let res = make(<list>);
  res := add!(res, node.value);
  for (ele in node.children)
    res := concatenate(res, traverse(ele));
  end;
  res;
end;

