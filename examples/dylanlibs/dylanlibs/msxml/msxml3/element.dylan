Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <element> (<node>)
  constant virtual slot element-tag-name :: <string>;
end class <element>;

define method proxy-class( interface :: <IXMLDOMElement> ) => (class == <element>)
  <element>
end proxy-class;

define macro dom-element-methods-definer
  { define dom-element-methods ?:name ?:body end }
   => { 
        define proxy-method (tag-name, ?name, ?name ## "/tagName") end;
        define proxy-method (get-attribute, ?name, ?name ## "/getAttribute") end;
        define proxy-method (set-attribute, ?name, ?name ## "/setAttribute") end;
        define proxy-method (remove-attribute, ?name, ?name ## "/removeAttribute") end;
        define proxy-method (get-attribute-node, ?name, ?name ## "/getAttributeNode") end;
        define proxy-method (set-attribute-node, ?name, ?name ## "/setAttributeNode") end;
        define proxy-method (remove-attribute-node, ?name, ?name ## "/removeAttributeNode") end;
        define proxy-method (get-elements-by-tag-name, ?name, ?name ## "/getElementsByTagName") end;
        define proxy-method (normalize, ?name, ?name ## "/normalize") end;
      }
end macro dom-element-methods-definer;

define dom-node-methods IXMLDOMElement end;
define dom-element-methods IXMLDOMElement end;

define method element-tag-name (element :: <element>) => (s :: <string>)
  convert-to-string(%tag-name(element.msxml3-interface));
end method element-tag-name;

define method element-get-attribute (element :: <element>, name :: <string>) => (r :: <object>)
  convert-to-safe-type(%get-attribute(element.msxml3-interface, name));
end method element-get-attribute;

define method element-set-attribute (element :: <element>, name :: <string>, value :: <object>) => ()
  %set-attribute(element.msxml3-interface, name, value);
end method element-set-attribute;

define method element-remove-attribute (element :: <element>, name :: <string>) => ()
  %remove-attribute(element.msxml3-interface);
end method element-remove-attribute;

define method element-get-attribute-node (element :: <element>, name :: <string>) => (r :: <attribute>)
  make-proxy(%get-attribute-node(element.msxml3-interface, name));
end method element-get-attribute-node;

define method element-set-attribute-node (element :: <element>, attr :: <attribute>) => (a :: <attribute>)
  make-proxy(%set-attribute-node(element.msxml3-interface, attr.msxml3-interface));
end method element-set-attribute-node;

define method element-remove-attribute-node (element :: <element>, attr :: <attribute>) => (a :: <attribute>)
  make-proxy(%remove-attribute-node(element.msxml3-interface, attr.msxml3-interface));
end method element-remove-attribute-node;

define method element-normalize (element :: <element>) => ()
  %normalize(element.msxml3-interface);
end method element-normalize;

define method element-get-elements-by-tag-name (element :: <element>, name :: <string>) => (s :: <sequence>)
  node-list-to-sequence(%get-elements-by-tag-name(element.msxml3-interface, name));
end method element-get-elements-by-tag-name;

