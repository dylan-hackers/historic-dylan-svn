Module:    display
Synopsis:  display the nodes once parsed
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
remodulized-by: Douglas M. Auclair

define method display-node(node :: <node>)
  for(n in node.node-children)
    display-node(n)
  end for;
end method display-node;

define method display-node(node :: <attribute>)
  format-out("Attribute: [%s] = [%s]\n", node.attribute-name, node.attribute-value);
  next-method();
end method display-node;

define method display-node(node :: <element>)
  format-out("Element: [%s]\n", node.element-tag-name);
  for(attribute in node.element-attributes)
    display-node(attribute)
  end for;
  next-method();
end method display-node;

define method display-node(node :: <text-node>)
  format-out("Text: [%s]\n", node.text);
  next-method();
end method display-node;

define method display-node(node :: <document>)
  format-out("Document:\n");
  next-method();
end method display-node;

