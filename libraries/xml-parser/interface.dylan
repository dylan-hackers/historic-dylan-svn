module: interface
author: Andreas Bogk, Chris Double
copyright: (c) 2001, LGPL
class-hierarchy-rearrangement: Douglas M. Auclair

// --- CHRIS'S DEF'S --  with additions by Doug
// now modified to conform a bit to Andreas' XML-syntax fns

define abstract class <xml> (<object>)
  constant slot name :: <symbol>, required-init-keyword: name:;
end class <xml>;

define abstract class <node> (<xml>)
  slot node-children = #[], init-keyword: children:;    
end class <node>;

define class <attribute> (<xml>)
  constant slot attribute-value :: <string> = "", init-keyword: value:;
end class <attribute>;

// not sealed to allow XML element tags subclasses of <element>
define open abstract class <element> (<node>, <sequence>)
  slot element-parent :: <node>, init-keyword: parent:;
  constant slot element-attributes :: <vector> = #[], 
    init-keyword: attributes:;
  constant virtual slot text;
end class <element>;

// We want <element> to behave as <sequence> for indexing, but not
// for equivalence comparisons
define method \=(e1 :: <element>, e2 :: <element>) => (ans :: <boolean>)
  e1 == e2;  // compare identity, do not do a deep compare.
end method \=;

define class <document> (<node>)
end class <document>;

define class <char-string> (<xml>)
  inherited slot name, init-value: #"chars";
  constant slot text :: <string>, required-init-keyword: text:;
end class <char-string>;

define class <entity-reference> (<xml>)
  constant virtual slot entity-value;
end class <entity-reference>;

define class <char-reference> (<xml>)
  constant slot char :: <character>, required-init-keyword: char:;
end class <char-reference>;

define method unfiltered-text(elt :: <element>) => (s :: <string>)
  apply(concatenate, map(text, choose(rcurry(instance?, <char-string>), 
                                      elt.node-children)));
end method unfiltered-text;

define constant $hex-digit = "0123456789abcdefABCDEF";
define constant $version-number = concatenate($letter, $digit, "_.:");

// removes leading and trailing blanks
define method text(elt :: <element>) => (s :: <string>)
  trim-string(elt.unfiltered-text);
end method text;

define function trim-string(s :: <sequence>) => (t :: <string>)
  let is-space? = rcurry(member?, $space);
  let ans = as(<string>, s);
  let start = 0;
  let stop = ans.size;
  while(stop > 1 & ans[stop - 1].is-space?) stop := stop - 1; end while;
  while(start < ans.size & ans[start].is-space?) start := start + 1; end while;
  copy-sequence(ans, start: start, end: stop);
end function trim-string;
