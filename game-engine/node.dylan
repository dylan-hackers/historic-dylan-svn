module: node

define abstract class <node> (<object>)
end class <node>;

define class <visitor> (<object>);

define class <container-node> (<object>)
  slot children;
end class <container-node>;

define generic visit(node :: <node>, visitor :: <visitor>);

