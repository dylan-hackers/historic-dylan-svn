Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <named-node-map> (<msxml-proxy>)
  constant virtual slot named-node-map-length :: <integer>;
end class <named-node-map>;

define method proxy-class( interface :: <IXMLDOMNamedNodeMap> ) => (class == <named-node-map>)
  <named-node-map>
end proxy-class;

define macro dom-named-node-map-methods-definer
  { define dom-named-node-map-methods ?:name ?:body end }
   => { 
        define proxy-method (get-named-item, ?name, ?name ## "/getNamedItem") end;
        define proxy-method (set-named-item, ?name, ?name ## "/setNamedItem") end;
        define proxy-method (remove-named-item, ?name, ?name ## "/removeNamedItem") end;
        define proxy-method (length, ?name, ?name ## "/length") end;
        define proxy-method (get-qualified-item, ?name, ?name ## "/getQualifiedItem") end;
        define proxy-method (remove-qualified-item, ?name, ?name ## "/removeQualifiedItem") end;
        define proxy-method (next-node, ?name, ?name ## "/nextNode") end;
        define proxy-method (reset, ?name, ?name ## "/reset") end;
      }
end macro dom-named-node-map-methods-definer;

define dom-named-node-map-methods IXMLDOMNamedNodeMap end;

define method named-node-map-get-named-item( p :: <named-node-map>, name :: <string> ) => (r :: <node>)
  make-proxy(%get-named-item(p.msxml3-interface, name));
end method named-node-map-get-named-item;

define method named-node-map-set-named-item( p :: <named-node-map>, new-item :: <node> ) => (r :: <node>)
  make-proxy(%set-named-item(p.msxml3-interface, new-item.msxml3-interface));
end method named-node-map-set-named-item;

define method named-node-map-remove-named-item( p :: <named-node-map>, name :: <string> ) => (r :: <node>)
  make-proxy(%remove-named-item(p.msxml3-interface, name));
end method named-node-map-remove-named-item;

define method named-node-map-length( p :: <named-node-map> ) => (r :: <integer>)
  %length(p.msxml3-interface);
end method named-node-map-length;

define method named-node-map-get-qualified-item( p :: <named-node-map>, base-name :: <string>,
                                                namespace-uri :: <string>) => (r :: <node>)
  make-proxy(%get-qualified-item(p.msxml3-interface, base-name, namespace-uri));
end method named-node-map-get-qualified-item;

define method named-node-map-remove-qualified-item( p :: <named-node-map>, base-name :: <string>,
                                                namespace-uri :: <string>) => (r :: <node>)
  make-proxy(%remove-qualified-item(p.msxml3-interface, base-name, namespace-uri));
end method named-node-map-remove-qualified-item;

define method named-node-map-next-node( p :: <named-node-map>) => (r :: <node>)
  make-proxy(%next-node(p.msxml3-interface));
end method named-node-map-next-node;

define method named-node-map-reset( p :: <named-node-map>) => ()
  %reset(p.msxml3-interface);
end method named-node-map-reset;

