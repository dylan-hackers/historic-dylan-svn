module: interface
author: Andreas Bogk, Chris Double
copyright: (c) 2001, LGPL
translated-into-its-own-module-by: Douglas M. Auclair

// Interface bindings into the xml-parser library

define abstract open class <xml-builder> (<object>)
end class <xml-builder>;

define class <xml-element> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot attributes :: <table>, init-keyword: attributes:;
  slot char-data :: <string>;
end class <xml-element>;

define method print-object(element :: <xml-element>, s :: <stream>) => ()
  format(s, "<%s", element.name);
  for(value keyed-by key in element.attributes)
    format(s, " %s='%s'", key, value);
  end for;
  format(s, ">\n");
end method print-object;

define class <xml-parse-error> (<error>)
  slot message;
  slot source-location;
end class <xml-parse-error>;

define open generic start-element(bldr :: <xml-builder>, elt :: <xml-element>);
define open generic end-element(bldr :: <xml-builder>, elt :: <xml-element>);

// --- CHRIS'S DEF'S --
// now modified to conform a bit to Andreas' XML-syntax fns

define class <node> (<object>)
  slot node-children = #[], init-keyword: children:;    
end class <node>;

define class <attribute> (<node>)
  slot attribute-name :: <string>, required-init-keyword: name:;
  slot attribute-value :: <string> = "", init-keyword: value:;
end class <attribute>;

define class <element> (<node>)
  slot element-tag-name :: <string>, required-init-keyword: tag-name:;  
  slot element-attributes :: <vector> = #[], init-keyword: attributes:;
end class <element>;

define class <text-node> (<node>)
  constant slot text :: <string>, required-init-keyword: text:;  
end class <text-node>;

define class <document> (<node>)
end class <document>;

define class <entity-reference> (<object>)
  slot value :: <string>, required-init-keyword: value:;
end class <entity-reference>;

define class <char-reference> (<object>)
  slot value :: <string>, required-init-keyword: value:;
end class <char-reference>;

// and constants as classes
define constant <letter> = 
    one-of('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
           'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
           'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
           'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

define constant <digit> = 
    one-of('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

define constant <hex-digit> = 
    one-of('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'A',
           'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E', 'f', 'F');

define constant <other-name-char> =
    one-of('.', '-', '_', ':');

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define constant <version-number> =
  type-union(<letter>, <digit>, one-of('_', '.', ':'));

define constant <all-chars> = type-union(<letter>, <digit>, <space>);

