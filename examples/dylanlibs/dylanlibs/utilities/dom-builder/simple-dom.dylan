Module:    simple-dom
Synopsis:  A very simple implementation of a Document Object Model.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// An element in the document object model.
define class <element> (<object>)
  slot element-parent :: false-or(<element>) = #f;
  slot element-children :: <vector> = make(<stretchy-vector>);
end;

// An a child to the element.
define generic add-child(e :: <element>, child :: <element>)
 => (child :: <element>);

define method add-child(e :: <element>, child :: <element>)
 => (child :: <element>)
  e.element-children := add!(e.element-children, child);
  child.element-parent := e;
  child;
end;

// The root class of the document object model. It represents
// the document itself and is like an before the first document
// root element that has no tag.
define class <document> (<element>)
  keyword parent: = #f;
end class <document>;

// A single attribute for an element.
define class <attribute> (<object>)
  slot attribute-parent :: false-or(<node-element>) = #f;
  slot attribute-key :: <symbol>, required-init-keyword: key:;
  slot attribute-value :: <object>, required-init-keyword: value:;
end;

// An element that has a tag and attributes
define class <node-element> (<element>)
  slot node-element-tag :: <symbol>, required-init-keyword: tag:;
  slot node-element-attributes :: <vector> = make(<stretchy-vector>);
end;

define generic add-attribute(e :: <node-element>, child :: <attribute>)
 => (child :: <attribute>);

define method add-attribute(e :: <node-element>, child :: <attribute>)
 => (child :: <attribute>)
  e.node-element-attributes := add!(e.node-element-attributes, child);
  child.attribute-parent := e;
  child;
end;

// Text contents of an element. Usually a child of a <node-element>.
define class <text-element> (<element>)
  slot text-element-text :: <string>, init-keyword: text:;
end;

