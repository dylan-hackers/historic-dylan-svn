Module:    collect
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  gathers all elements of shape S (a sequence of symbols)

define class <collect-state> (<xform-state>)
  constant slot pattern :: <sequence>, required-init-keyword: pattern:;
  class slot elements :: <list> = #();
  slot candidate :: false-or(<element>) = #f;
//  slot locked-in? :: <boolean> = #f;
  slot depth :: <integer> = 0;
end class <collect-state>;

define variable *original-state* = #f;

define function copy-down(c :: <collect-state>, elt :: <element>)
 => (d :: <collect-state>)
  let col = make(<collect-state>, pattern: c.pattern.tail);
  unless(c.candidate)
    col.candidate := elt;
    //  col.locked-in? := #t;
    col.depth := *xml-depth*;
  end unless;
  col;
end function copy-down;

define method transform(elt :: <element>, tag-name :: <symbol>,
                        state :: <collect-state>, str :: <stream>)
  let new-state = if(tag-name == state.pattern[0])
             if(state.pattern.size == 1)
               state.elements := concatenate(state.elements, list(state.candidate | elt));
               *original-state*;
             else
               copy-down(state, elt);
             end if;
           else
             if(*xml-depth* > state.depth) state else *original-state*; end if;
           end if;
  next-method(elt, tag-name, new-state, str);
end method transform;
      
define function collect-elements(in :: <document>, tree :: <sequence>)
 => (ans :: <sequence>)
  *original-state* := make(<collect-state>, 
                           pattern: as(<list>, 
                                       map(curry(as, <symbol>), tree)));
  transform-document(in, state: *original-state*);
  *original-state*.elements;
end function collect-elements;

/***
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
**/
