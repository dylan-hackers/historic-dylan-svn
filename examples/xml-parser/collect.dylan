Module:    collect
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  gathers all elements of shape S (a sequence of symbols)

// think next of implementing * (the wildcard matcher) where
// sym == * is always #t

/** something like:
define class <wildcard> (<object>) end;
define method \==(sym :: <symbol>, w :: <wildcard>) => (b :: <boolean>)
  #t;
end;
define method \==(w :: <wildcard>, sym :: <symbol>) => (b :: <boolean>)
  #t;
end;

and do the transformation on the match list to map "*" => <wildcard>
**/

// here's an example of a state more complex than a <symbol> (which
// the other state uses in xml-test/ wind up to be).  The class slot
// elements grows incrementally as the XML tree is traversed.  The
// slot pattern tracks the match during the tree descent.
define class <collect-state> (<xform-state>)
  constant slot pattern :: <sequence>, required-init-keyword: pattern:;
  virtual /* class */ slot elements :: <sequence>;
  slot candidate :: false-or(<element>) = #f;
  slot depth :: <integer> = 0;
end class <collect-state>;

// elements is a class slot, but the GwydionDylan compiler currently
// pukes on class slots, so I'm using the virtual slot work-around
// that Gabor gave me:
define variable *elements* :: <sequence> = #();
define method elements(c :: <collect-state>) => (seq :: <sequence>)
  *elements*;
end method elements;

define method elements-setter(seq :: <sequence>, c :: <collect-state>)
 => (seq1 :: <sequence>)
  *elements* := seq;
end method elements-setter;

define variable *original-state* :: false-or(<collect-state>) = #f;

define function copy-down(c :: <collect-state>, elt :: <element>)
 => (d :: <collect-state>)
  let col :: <collect-state> = make(<collect-state>, pattern: c.pattern.tail);
  unless(c.candidate)
    col.candidate := elt;
    col.depth := *xml-depth*;
  end unless;
  col;
end function copy-down;

define method transform(elt :: <element>, tag-name :: <symbol>,
                        state :: <collect-state>, str :: <stream>)
  let new-state = if(tag-name == state.pattern[0])
             if(state.pattern.size == 1)
               state.elements := concatenate(state.elements, 
					     list(state.candidate | elt));
               *original-state*;
             else
               copy-down(state, elt);
             end if;
           else
             if(*xml-depth* > state.depth) state else *original-state*; end if;
           end if;
  next-method(elt, tag-name, new-state, str);
end method transform;

// takes an element or a document and a sequence (currently
// of the XSL form "//elt/elt1/elt2/...eltN") and returns
// the list of elements that satisfy that sequence shape
define generic collect-elements(in :: <node>, tree :: <sequence>)
 => (ans :: <sequence>);
      
define method collect-elements(in :: <document>, tree :: <sequence>)
 => (ans :: <sequence>)
  *original-state* := make(<collect-state>, 
                           pattern: as(<list>, 
                                       map(curry(as, <symbol>), tree)));
  *original-state*.elements := #();
  transform-document(in, state: *original-state*);
  *original-state*.elements;
end method collect-elements;

// this method allows us to specify the search start from a specific
// element, e.g.: "/company-xyz/employees/managers/joe/assistant/daniel"
// instead of looking for any J. Random "//daniel"
define method collect-elements(elt :: <element>, tree :: <sequence>)
 => (ans :: <sequence>)
  collect-elements(make(<document>, 
                        name: elt.name, 
                        children: vector(elt)),
                   tree);
end method collect-elements;

//-------------------------------------------------------
// indexing
// an element may have 0, 1, or more elements of kid-name
define function element-children(elt :: <element>, kid-name :: <symbol>)
 => (ans :: <sequence>)
  let choose-elements
  = method(elt :: <element>) => (seq :: <sequence>)
      choose(rcurry(instance?, <element>), elt.node-children);
    end method;
  choose(compose(curry(\==, kid-name), name), elt.choose-elements);
end function element-children;

// however, each attribute name must be unique
define function attribute-value(elt :: <element>, 
                                attrib-name :: <symbol>)
 => (ans :: false-or(<string>))
  any?(method(x) x.name == attrib-name & x.value end, 
       elt.element-attributes);
end function attribute-value;

// this method allows us to index into <element>s as if they were
// <table>s ... it returns the appropriate element OR attribute value
// depending on the request, e.g. <person name="Doug"/> allows
// elt["@name"] => "Doug" when elt is at {<element>, name=#"person"}
//
// For the following XML:
// <article>
//  <title>Dynamic Namespace</title>
//  <text>Synopsis:  Using libraries as units of compilation and
// modules as namespaces affords ...</text>
//  <text>The standard view of namespaces is that they are a static
// thing, their names do not change, nor do the bindings within 
// them.</text>
//  <text>Avoiding name clashes under these assumptions can lead
// to contorted code...</text>
// </article>
//
// and elt is {<element>, name: #"article"} then
// elt["title"] returns one element and elt["text"] returns
// a sequence of three elements
define method element(elt :: <element>, 
                      key :: type-union(<string>, <symbol>),
                      #key default, always-sequence?)
 => (ans)
  let string = as(<string>, key);
  if(string[0] == '@')
    attribute-value(elt, as(<symbol>, copy-sequence(string, start: 1)));
  else
    let kids = element-children(elt, as(<symbol>, key));
// let's simplify indexing for unique tags
    if(kids.size == 1 & ~ always-sequence?) kids[0] else kids end if;
  end if;
end method element;
