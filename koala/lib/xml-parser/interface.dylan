module: interface
author: Douglas M. Auclair
inspired-from-the-work-by: Andreas Bogk, Chris Double
copyright: (c) 2001-2002, LGPL

define abstract class <xml> (<object>)
  virtual slot name;
  slot name-with-proper-capitalization :: <string>,
    required-init-keyword: name:;
end class <xml>;

define method name(xml :: <xml>) => (symbol)
  as(<symbol>, xml.name-with-proper-capitalization);
end method name;

define method name-setter(sym, xml :: <xml>) => (ans)
  xml.name-with-proper-capitalization := as(<string>, sym);
end method name-setter;

define method initialize(xml :: <xml>, #key name) => (elt :: <xml>)
  unless(slot-initialized?(xml, name-with-proper-capitalization)
         & ~ xml.name-with-proper-capitalization.empty?)
    xml.name-with-proper-capitalization := as(<string>, name);
  end unless;
  xml;
end method initialize;

define abstract class <node-mixin> (<object>)
  slot node-children = #[], init-keyword: children:;    
end class <node-mixin>;

define class <attribute> (<xml>)
  slot attribute-value :: <string> = "", init-keyword: value:;
end class <attribute>;

define abstract class <tag> (<xml>)

    // The each-subclass adjective should be commented out for this to
    // compile in the current Gwydion Dylan release (2.3.9pre1).  The
    // next release will fix this problem, I believe.  -Carl Gay

  constant each-subclass slot after-open :: <string> = "";
  constant each-subclass slot before-close :: <string> = "";
end class <tag>;

define abstract class <attributes> (<tag>)
  slot attributes :: <vector> = #[], init-keyword: attributes:;
end class <attributes>;

// a document contains pi's, comments and the root node
define class <document> (<xml>, <node-mixin>)
  inherited slot name-with-proper-capitalization = "ignored";
  constant virtual slot root :: <element>;
end class <document>;

define method name(doc :: <document>) => (symbol)
  doc.root.name;
end method name;

define method name-setter(name, doc :: <document>) => (ans)
  doc.root.name-with-proper-capitalization := as(<string>, name);
  name;
end method name-setter;

define method root(doc :: <document>) => (elt :: <element>)
  choose(rcurry(instance?, <element>), doc.node-children)[0]
end method root;

// An element is a thing that has attributes and children.
// Not sealed to allow XML element tags subclasses of <element>
//
// N.B. it is (very) preferable to use make-element instead of make
// when working in conjuction with the xml-parser library
define open class <element> (<attributes>, <node-mixin>, <mutable-collection>)
  slot element-parent :: <node-mixin>, init-keyword: parent:;
  virtual slot text;
end class <element>;

// We want <element> to behave as <sequence> for indexing, but not
// for equivalence comparisons
define method \=(e1 :: <element>, e2 :: <element>)
 => (ans :: <boolean>)
  e1 == e2;  // compare identity, do not do a deep compare.
end method \=;

// allows users to interpose their own object hierarchies for the elements
// This is sort of CLOS's change-class limited to compile-time schemes
define open generic make-element(kids :: <sequence>, name :: <symbol>, 
                                 attribs :: <sequence>, mod :: <boolean>)
 => (elt :: <element>);

define class <char-string> (<xml>)
  inherited slot name-with-proper-capitalization = "chars";
  constant slot text :: <string>, required-init-keyword: text:;
end class <char-string>;

// added this superclass to share common functionality of references,
// especially in the printing module.
define abstract class <reference> (<xml>) end;
define class <entity-reference> (<reference>)
  constant virtual slot entity-value;
end class <entity-reference>;

define variable *entities* :: <table> = make(<table>);

define method entity-value(ent :: <entity-reference>) => (val :: <sequence>)
  *entities*[ent.name];
end method entity-value;

define class <char-reference> (<reference>)
  constant slot char :: <character>, required-init-keyword: char:;
end class <char-reference>;

define method unfiltered-text(elt :: <element>) => (s :: <string>)
  let strs = choose(rcurry(instance?, <char-string>), elt.node-children);
  if(strs.empty?) "" else apply(concatenate, map(text, strs)) end if;
end method unfiltered-text;

define constant $hex-digit = "0123456789abcdefABCDEF";
define constant $version-number = concatenate($letter, $digit, "_.:");

// removes leading and trailing blanks
define method text(elt :: <element>) => (s :: <string>)
  trim-string(elt.unfiltered-text);
end method text;

// a bit of syntactic sugar instead of finding the <char-string> in the elt
define method text-setter(txt :: <string>, elt :: <element>) => (s :: <string>)
  // WARNING:  we're only replacing the first <char-string> we find!
  // use element-setter(foo, elt.node-children, x) for more precise control
  
  // if there's no text in this element, then we'll put the text at the end!!!
  let index = find-key(elt.node-children, rcurry(instance?, <char-string>),
                       failure: #f);
  let the-txt = make(<char-string>, text: txt);
  if(index)
    elt.node-children[index] := the-txt;
  else
    elt.node-children := concatenate(elt.node-children, vector(the-txt));
  end if;
  txt;
end method text-setter;

define function trim-string(s :: <sequence>) => (t :: <string>)
  let is-space? = rcurry(member?, $space);
  let ans = as(<string>, s);
  let start = 0;
  let stop = ans.size;
  while(stop > 1 & ans[stop - 1].is-space?) stop := stop - 1; end while;
  while(start < ans.size & ans[start].is-space?) start := start + 1; end while;
  copy-sequence(ans, start: start, end: stop);
end function trim-string;

// these types are not put into the <document> when processing an xml document
// they are simply here for printing convenience

// processing instructions do not have any of children, text or parents
define class <processing-instruction> (<attributes>)
  inherited slot after-open = "?";
  inherited slot before-close = "?";
end class <processing-instruction>;

define abstract class <external-mixin> (<object>)
  constant slot sys-id :: false-or(<string>) = #f, init-keyword: sys-id:;
  constant slot pub-id :: false-or(<string>) = #f, init-keyword: pub-id:;
  constant slot sys/pub :: one-of(#"system", #"public", #f) = #f, init-keyword: sys/pub:;
end class <external-mixin>;

define abstract class <entity> (<tag>)
  inherited slot after-open = "!ENTITY ";
end class <entity>;

// e.g. <!ENTITY foo "<wow>Neat!</wow>"> has a sequence of 1 element:
// <wow>Neat!</wow> (where "Neat!" is a <char-string> in <wow>)
define class <internal-entity> (<entity>)
  slot expansion :: <sequence> = #[], init-keyword: expands-to:;
end class <internal-entity>;

define class <external-entity> (<entity>, <external-mixin>) end;

define class <dtd> (<tag>, <external-mixin>)
  slot internal-declarations :: <sequence> = #[], init-keyword: internal:;
  inherited slot after-open = "!DOCTYPE ";
end class <dtd>;

define class <comment> (<tag>)
  inherited slot name-with-proper-capitalization = "-";
  inherited slot after-open = "!-";
  inherited slot before-close = " --";
  slot comment :: <string> = "", init-keyword: comment:;
end class <comment>;
