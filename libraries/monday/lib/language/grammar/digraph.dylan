Module: digraph


define method find-rooted-scc
    (root :: <object>,
     visit-function :: <function>,
     collapse-function :: <function>) => ();
  let stack :: <list> = #();
  let stack-depth :: <integer> = 0;
  let index :: <object-table> = make(<object-table>);

  
local
  method traverse (node :: <object>) => ();
    stack := add!(stack, node);
    stack-depth := stack-depth + 1;
    let initial-stack-depth = stack-depth;
    index[node] := initial-stack-depth;
    
visit-function(node,
               method(child :: <object>) => ();
                   if (element(index, child, default: 0) = 0)
                     traverse(child);
                   end if;

                   let child-index = index[child];
                   let node-index = index[node];
                   if (child-index)
                     if (node-index)
                       index[node] := min(node-index, child-index);
                     else
                       index[node] := child-index;
                     end if;
                   end if;
               end method);
            
    
if (index[node] = initial-stack-depth)
  while (first(stack) ~== node)
    collapse-function(node, first(stack));
          
    index[first(stack)] := #f;
    stack := tail(stack);
    stack-depth := stack-depth - 1;
  end while;

  index[node] := #f;
  stack := tail(stack);
  stack-depth := stack-depth - 1;
end if;
            
  end method;
            
  traverse(root);
end method;
            
