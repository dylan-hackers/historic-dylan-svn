Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <document2> (<document>)
  constant virtual slot document-namespaces :: <schema-collection>;
  constant virtual slot document-schemas :: <object>;
end class <document2>;

define method proxy-class( interface :: <IXMLDOMDocument2> ) => (class == <document2>)
  <document2>
end proxy-class;

define macro dom-document2-methods-definer
  { define dom-document2-methods ?:name ?:body end }
   => { 
        define proxy-method (namespaces, ?name, ?name ## "/namespaces") end;
        define proxy-method (schemas, ?name, ?name ## "/schemas") end;
        define proxy-method (validate, ?name, ?name ## "/validate") end;
        define proxy-method (set-property, ?name, ?name ## "/setProperty") end;
        define proxy-method (get-property, ?name, ?name ## "/getProperty") end;
      }
end macro dom-document2-methods-definer;

define dom-node-methods IXMLDOMDocument2 end;
define dom-document-methods IXMLDOMDocument2 end;
define dom-document2-methods IXMLDOMDocument2 end;

define method document-namespaces (p :: <document2>) => (result :: <schema-collection>)
  make-proxy(%namespaces(p.msxml3-interface));
end method document-namespaces;

define method document-schemas (p :: <document2>) => (result :: <object>)
  convert-to-safe-type(%schemas(p.msxml3-interface));
end method document-schemas;

define method document-validate (p :: <document2>) => (result :: <parse-error>)
  make-proxy(%validate(p.msxml3-interface));
end method document-validate;

define method document-set-property (p :: <document2>, name :: <string>, value :: <object>) => ()
  %set-property(p.msxml3-interface, name, value);
end method document-set-property;

define method document-get-property (p :: <document2>, name :: <string>) => (r :: <object>)
  convert-to-safe-type(%get-property(p.msxml3-interface, name));
end method document-get-property;

