Module:    collect
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  gathers all elements of shape S (a sequence of symbols)

define function choose-elements(elt :: <element>) => (seq :: <sequence>)
  choose(rcurry(instance?, <element>), elt.node-children);
end function choose-elements;
  
// an element may have 0, 1, or more elements of kid-name
define function element-children(elt :: <element>, kid-name :: <symbol>)
 => (ans :: <sequence>)
  choose(compose(curry(\==, kid-name), name), elt.choose-elements);
end function element-children;

// however, each attribute name must be unique
define function attribute-value(elt :: <element>, attrib-name :: <symbol>)
 => (ans :: <string>)
  any?(method(x) x.name == attrib-name & x.value end, 
       elt.element-attributes);
end function attribute-value;

// this method allows us to index into <element>s as if they were
// <table>s ... it returns the appropriate element OR attribute value
// depending on the request, e.g. <person name="Doug"/> allows
// elt["@name"] => "Doug" when elt is at {<element>, name=#"person"}
define method element(elt :: <element>, 
     key :: type-union(<string>, <symbol>), #key default)
 => (ans)
  if(key[0] == '@')
    attribute-value(elt, as(<symbol>, copy-sequence(key, start: 1)));
  else
    element-children(elt, as(<symbol>, key));
  end if;
end method element;

// for now, let's only concern ourselves with an element hierarchy
// (currently, we only care about attibute values, not placement)

define function collect-elements(in :: <document>, tree :: <list>)
 => (ans :: <sequence>)
  local method scanner(stor :: <list>, elmt :: <element>,
                       target :: <symbol>) => (ans :: <list>)
    if(elmt.name == target) 
      stor := concatenate(stor, list(elmt))
    end if;
    if(elmt.node-children.empty?) 
      stor
    else 
      reduce(rcurry(scanner, target), stor, elmt.choose-elements);
    end if;
  end method;

  let candidates = scanner(#(), in.node-children[0], tree.head);

  local method match?(elt :: <element>, branch :: <list>)
   => (ans :: <boolean>)
    elt.name == branch.head 
    & (branch.tail.empty? 
       | any?(rcurry(match?, branch.tail), elt.choose-elements));
  end method;

  choose(rcurry(match?, tree), candidates);
end function collect-elements;
