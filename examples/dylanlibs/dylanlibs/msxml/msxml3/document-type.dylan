Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <document-type> (<node>)
  constant virtual slot document-type-name :: <string>;
  constant virtual slot document-type-entities :: <named-node-map>;
  constant virtual slot document-type-notations :: <named-node-map>;
end class <document-type>;

define method proxy-class( interface :: <IXMLDOMDocumentType> ) => (class == <document-type>)
  <document-type>
end proxy-class;

define macro dom-document-type-methods-definer
  { define dom-document-type-methods ?:name ?:body end }
   => { 
        define proxy-method (name, ?name, ?name ## "/name") end;
        define proxy-method (entities, ?name, ?name ## "/entities") end;
        define proxy-method (notations, ?name, ?name ## "/notations") end;
      }
end macro dom-document-type-methods-definer;

define dom-node-methods IXMLDOMDocumentType end;
define dom-document-type-methods IXMLDOMDocumentType end;

define method document-type-name( p :: <document-type> ) => (r :: <string>)
  convert-to-string(%name(p.msxml3-interface));
end method document-type-name;

define method document-type-entities( p :: <document-type> ) => (r :: <named-node-map>)
  make-proxy(%entities(p.msxml3-interface));
end method document-type-entities;

define method document-type-notations( p :: <document-type> ) => (r :: <named-node-map>)
  make-proxy(%notations(p.msxml3-interface));
end method document-type-notations;

