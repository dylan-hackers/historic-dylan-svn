module: xml-parser
synopsis: walk DOM tree and print node names
author: Andreas Bogk <andreas@andreas.org>
copyright: (c) 2000 Gwydion Dylan Maintainers. Licensed under LGPL

define method is-null?(pointer :: <statically-typed-pointer>)
  as(<statically-typed-pointer>, 0) == as(<statically-typed-pointer>,pointer);
end method is-null?;

define method is-not-null?(pointer :: <statically-typed-pointer>)
  as(<statically-typed-pointer>, 0) ~== as(<statically-typed-pointer>, pointer);
end method is-not-null?;

define method print-node(xml-node)
  format-out("Node name is %s\n", xml-node.get-name);
  force-output(*standard-output*);
  if(is-not-null?(xml-node.get-children))
    print-node(xml-node.get-children);
  end if;
  let node = xml-node.get-next;
  while(is-not-null?(node))
    format-out("Node name is %s\n", node.get-name);
    force-output(*standard-output*);
    if(is-not-null?(node.get-children))
      print-node(node.get-children);
    end if;
    node := node.get-next;
  end while;
end method print-node;

define method parse(filename)
  let document = xml-Parse-File(filename);

  format-out("Document name is %s\n", document.get-name);
  print-node(document.get-children);
end method;

parse(application-arguments()[0]);
