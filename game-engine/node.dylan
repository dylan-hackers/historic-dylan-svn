module: vrml-model

define abstract class <node> (<object>)
end class <node>;

define class <container-node> (<node>)
  slot children = #(), init-keyword: children:;
end class <container-node>;

define class <indexed-face-set> (<node>)
  slot ccw :: <boolean> = #f, init-keyword: ccw:; // orientation of faces
  slot points, init-keyword: points:;
  slot polygon-indices, init-keyword: indices:;
end class <indexed-face-set>;

define class <transform> (<container-node>)
  slot scale = #f, init-keyword: scale:;
  slot translation = #f, init-keyword: translate:;
end class <transform>;

define class <line-grid> (<node>)
end class <line-grid>;

define class <sphere> (<node>)
end class <sphere>;

define generic preorder-traversal(node :: <node>, function :: <function>);

define method preorder-traversal(node :: <node>, function :: <function>)
  function(node);
end method preorder-traversal;

define method preorder-traversal(node :: <container-node>, 
                                 function :: <function>)
  function(node);
  for(i in node.children)
    preorder-traversal(i, function);
  end for;
end method preorder-traversal;

