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
  class slot elements :: <sequence>;
  slot candidate :: false-or(<element>) = #f;
  slot depth :: <integer> = 0;
end class <collect-state>;

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
define generic collect-elements(in :: <node-mixin>, tree :: <sequence>)
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
  collect-elements(make(<document>, children: vector(elt)), tree);
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
define function attribute(elt :: <element>, attrib-name :: <string>)
 => (ans :: false-or(<attribute>))
  let attrib-sym = as(<symbol>, copy-sequence(attrib-name, start: 1));
  any?(method(x) x.name == attrib-sym & x end, elt.attributes);
end function attribute;

define class <not-initialized> (<object>) end;
define constant $not-initialized = make(<not-initialized>);

// this method allows us to index into <element>s as if they were
// <table>s ... it returns the appropriate element(s) OR attribute value
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
// (and elt["text"][1] returns "The standard view [...]")
define method element(elt :: <element>, 
                      key :: type-union(<string>, <symbol>),
                      #key default = $not-initialized, always-sequence? = #f)
 => (ans)
  let string = as(<string>, key);
  let ans = #f;
  if(string[0] == '@')
    ans := aif(attribute(elt, string)) it.value else #f end aif;
  else
    let kids = element-children(elt, as(<symbol>, key));
// let's simplify indexing for unique tags
    unless(kids.size == 0)
      ans := if(kids.size == 1 & ~ always-sequence?) kids[0] else kids end if;
    end unless;
  end if;
  if(ans)
    ans
  else if(default == $not-initialized)
         error("%s element not found in %s", key, elt.name);
       else
         default
       end if;
  end if;
end method element;

// a twist on element-setter:
// <foo name='mumble'><bar>baz</bar></foo> (where elt is <foo>)
// elt["bar"] := "quux" yields <foo name='mumble'><bar>quux</bar></foo>
// and elt["@name"] := "yo" yield <foo name='yo'><bar>quux</bar></foo>
define method element-setter(txt :: <string>, elt :: <element>,
                             key :: type-union(<string>, <symbol>))
 => (txt-back)
  let string = as(<string>, key);
  let setter-fn = #f;
  let ans = #f;
  if(string[0] == '@')
    ans := attribute(elt, string);
    setter-fn := value-setter;
  else
    ans := elt[key];
    setter-fn := text-setter;
  end if;
  if(ans & setter-fn) 
    setter-fn(txt, ans)
  else
    error("%s element not in %s", key, elt.name);
  end if;
  txt
end method element-setter;

// and the more usual:
// elt["bar"] := make(<element>, name: "baz", children: #[], 
//                    parent: elt, attributes: #[]) 
// yields <foo name='yo'><baz/></foo>, and
// elt["@name"] := make(<attribute>, name: "color", value: "blue")
// yields <foo color='blue'><baz/></foo>
define method element-setter(new-elt :: <xml>, elt :: <element>,
                             key :: type-union(<string>, <symbol>))
 => (newer)
  let string = as(<string>, key);
  if(string[0] == '@')
    let sym = as(<symbol>, copy-sequence(string, start: 1));
    let index = find-key(elt, method(x) x.name == sym end, failure: #f);
    if(index) elt.attributes[index] := new-elt
    else error("No attribute %s in element %s", sym, elt.name);
    end if;
  else
    // force error if we cannot find a key:
    elt[key];
    let sym = as(<symbol>, key);
    // replace the first occurence
    let index = find-key(elt, method(x) x.name == sym end);
    elt.node-children[index] := new-elt;
  end if;
  new-elt;
end method element-setter;
