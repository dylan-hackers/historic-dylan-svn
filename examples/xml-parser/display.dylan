Module:    display
Synopsis:  displaying a parsed xml document.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
synthesis: Douglas M. Auclair

define generic display-node(node);

define method display-node(node :: <char-string>)
  format-out("Text: [%s]\n", node.text);
end method display-node;

define method display-node(node :: <entity-reference>)
  format-out("Entity: [%s = %s]\n", node.name, node.entity-value);
end method display-node;

define method display-node(node :: <char-reference>)
  format-out("Character: [%s = '%c']\n", node.name, node.char);
end method display-node;

define method display-node(node :: <node>)
  for(n in node.node-children)
    display-node(n)
  end for;
end method display-node;

define method display-node(node :: <attribute>)
  format-out("Attribute: [%s] = [%s]\n", node.name,
             node.attribute-value);
end method display-node;

define method display-node(node :: <element>)
  format-out("Element: [%s]\n", node.name);
  for(attribute in node.element-attributes)
    display-node(attribute)
  end for;
  next-method();
end method display-node;

define method display-node(node :: <document>)
  format-out("Document:\n");
  next-method();
end method display-node;

