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

define class <collect-state> (<xform-state>)
  constant slot pattern :: <sequence>, required-init-keyword: pattern:;
  class slot elements :: <list> = #();
  slot candidate :: false-or(<element>) = #f;
  slot depth :: <integer> = 0;
end class <collect-state>;

define variable *original-state* = #f;

define function copy-down(c :: <collect-state>, elt :: <element>)
 => (d :: <collect-state>)
  let col = make(<collect-state>, pattern: c.pattern.tail);
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

