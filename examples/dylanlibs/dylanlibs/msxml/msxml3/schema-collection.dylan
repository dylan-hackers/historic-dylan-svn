Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <schema-collection> (<msxml-proxy>)
  constant virtual slot schema-collection-length :: <integer>;
end class <schema-collection>;

define method proxy-class( interface :: <IXMLDOMSchemaCollection> ) => (class == <schema-collection>)
  <schema-collection>
end proxy-class;

define macro dom-schema-collection-methods-definer
  { define dom-schema-collection-methods ?:name ?:body end }
   => { 
        define proxy-method (add, ?name, ?name ## "/add") end;
        define proxy-method (get, ?name, ?name ## "/get") end;
        define proxy-method (remove, ?name, ?name ## "/remove") end;
        define proxy-method (length, ?name, ?name ## "/length") end;
        define proxy-method (add-collection, ?name, ?name ## "/addCollection") end;
      }
end macro dom-schema-collection-methods-definer;

define dom-schema-collection-methods IXMLDOMSchemaCollection end;

define method schema-collection-add (p :: <schema-collection>, namespaceURI :: <string>, 
    var :: <object>) => ()
  %add(p.msxml3-interface, namespaceURI, var);
end method schema-collection-add;

define method schema-collection-get (p :: <schema-collection>, namespaceURI :: <string>) => (r :: <node>)
  make-proxy(%get(p.msxml3-interface, namespaceURI));
end method schema-collection-get;

define method schema-collection-remove (p :: <schema-collection>, namespaceURI :: <string>) => ()
  %remove(p.msxml3-interface, namespaceURI);
end method schema-collection-remove;

define method schema-collection-length (p :: <schema-collection>) => (r :: <integer>)
  %length(p.msxml3-interface);
end method schema-collection-length;

define method schema-collection-add-collection (p :: <schema-collection>, other :: <schema-collection>) => ()
  %add-collection(p.msxml3-interface);
end method schema-collection-add-collection;

