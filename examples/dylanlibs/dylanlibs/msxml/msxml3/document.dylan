Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <document> (<node>)
  constant virtual slot document-doctype :: <document-type>;
  constant virtual slot document-implementation :: <implementation>;
  constant virtual slot document-element :: <element>;
  constant virtual slot document-ready-state :: <integer>;
  constant virtual slot document-parse-error :: <parse-error>;
  constant virtual slot document-url :: <string>;
  virtual slot document-async? :: <boolean>;
  virtual slot document-validate-on-parse? :: <boolean>;
  virtual slot document-resolve-externals? :: <boolean>;
  virtual slot document-preserve-whitespace? :: <boolean>;
end class <document>;

define method proxy-class( interface :: <IXMLDOMDocument> ) => (class == <document>)
  <document>
end proxy-class;

define macro dom-document-methods-definer
  { define dom-document-methods ?:name ?:body end }
   => { 
        define proxy-method (doctype, ?name, ?name ## "/doctype") end;
        define proxy-method (implementation, ?name, ?name ## "/implementation") end;
        define proxy-method (document-element, ?name, ?name ## "/documentElement") end;
        define proxy-method (create-element, ?name, ?name ## "/createElement") end;
        define proxy-method (create-document-fragment, ?name, ?name ## "/createDocumentFragment") end;
        define proxy-method (create-text-node, ?name, ?name ## "/createTextNode") end;
        define proxy-method (create-comment, ?name, ?name ## "/createComment") end;
        define proxy-method (create-CDATA-section, ?name, ?name ## "/createCDATASection") end;
        define proxy-method (create-processing-instruction, ?name, ?name ## "/createProcessingInstruction") end;
        define proxy-method (create-attribute, ?name, ?name ## "/createAttribute") end;
        define proxy-method (create-entity-reference, ?name, ?name ## "/createEntityReference") end;
        define proxy-method (get-elements-by-tag-name, ?name, ?name ## "/getElementsByTagName") end;
        define proxy-method (create-node, ?name, ?name ## "/createNode") end;
        define proxy-method (node-from-id, ?name, ?name ## "/nodeFromID") end;
        define proxy-method (load, ?name, ?name ## "/load") end;
        define proxy-method (ready-state, ?name, ?name ## "/readyState") end;
        define proxy-method (parse-error, ?name, ?name ## "/parseError") end;
        define proxy-method (url, ?name, ?name ## "/url") end;
        define proxy-method (async, ?name, ?name ## "/async") end;
        define proxy-method-setter (async-setter, ?name, ?name ## "/async") end;
        define proxy-method (abort, ?name, ?name ## "/abort") end;
        define proxy-method (load-xml, ?name, ?name ## "/loadXML") end;
        define proxy-method (save, ?name, ?name ## "/save") end;
        define proxy-method (validate-on-parse, ?name, ?name ## "/validateOnParse") end;
        define proxy-method-setter (validate-on-parse-setter, ?name, ?name ## "/validateOnParse") end;
        define proxy-method (resolve-externals, ?name, ?name ## "/resolveExternals") end;
        define proxy-method-setter (resolve-externals-setter, ?name, ?name ## "/resolveExternals") end;
        define proxy-method (preserve-white-space, ?name, ?name ## "/preserveWhiteSpace") end;
        define proxy-method-setter (preserve-white-space-setter, ?name, ?name ## "/preserveWhiteSpace") end;
//        define proxy-method (on-ready-state-change, ?name, ?name ## "/onreadystatechange") end;
//        define proxy-method (on-data-available, ?name, ?name ## "/ondataavailable") end;
//        define proxy-method (on-transform-node, ?name, ?name ## "/ontransformnode") end;
      }
end macro dom-document-methods-definer;

define dom-node-methods IXMLDOMDocument end;
define dom-document-methods IXMLDOMDocument end;

define method document-doctype (p :: <document>) => (result :: <document-type>)
  make-proxy(%doctype(p.msxml3-interface));
end method document-doctype;

define method document-implementation (p :: <document>) => (result :: <implementation>)
  make-proxy(%implementation(p.msxml3-interface));
end method document-implementation;

define method document-element (p :: <document>) => (result :: <element>)
  make-proxy(%document-element(p.msxml3-interface));
end method document-element;

define method document-create-element( p :: <document>, tag-name :: <string>) => (r :: <element>)
  make-proxy(%create-element(p.msxml3-interface, tag-name)); 
end method document-create-element;

define method document-create-document-fragment( p :: <document>) => (r :: <document-fragment>)
  make-proxy(%create-document-fragment(p.msxml3-interface)); 
end method document-create-document-fragment;

define method document-create-text-node( p :: <document>, data :: <string>) => (r :: <text>)
  make-proxy(%create-text-node(p.msxml3-interface, data)); 
end method document-create-text-node;

define method document-create-comment( p :: <document>, data :: <string>) => (r :: <comment>)
  make-proxy(%create-comment(p.msxml3-interface, data)); 
end method document-create-comment;

define method document-create-cdata-section( p :: <document>, data :: <string>) => (r :: <cdata-section>)
  make-proxy(%create-cdata-section(p.msxml3-interface, data)); 
end method document-create-cdata-section;

define method document-create-processing-instruction( p :: <document>, target :: <string>, data :: <string>) => (r :: <processing-instruction>)
  make-proxy(%create-processing-instruction(p.msxml3-interface, target, data)); 
end method document-create-processing-instruction;

define method document-create-attribute( p :: <document>, name :: <string>) => (r :: <attribute>)
  make-proxy(%create-attribute(p.msxml3-interface, name)); 
end method document-create-attribute;

define method document-create-entity-reference( p :: <document>, name :: <string>) => (r :: <entity-reference>)
  make-proxy(%create-entity-reference(p.msxml3-interface, name)); 
end method document-create-entity-reference;

define method document-get-elements-by-tag-name( p :: <document>, tag-name :: <string>) => (r :: <sequence>)
  node-list-to-sequence(%get-elements-by-tag-name(p.msxml3-interface, tag-name)); 
end method document-get-elements-by-tag-name;

define method document-create-node( p :: <document>, type :: <object>, name :: <string>, namespaceURI :: <string>) => (r :: <node>)
  make-proxy(%create-node(p.msxml3-interface, type, name, namespaceURI)); 
end method document-create-node;

define method document-node-from-id( p :: <document>, id :: <string>) => (r :: <node>)
  make-proxy(%node-from-id(p.msxml3-interface, id)); 
end method document-node-from-id;

define method document-load( p :: <document>, source :: <object>) => (r :: <boolean>)
  %load(p.msxml3-interface, source); 
end method document-load;

define method document-ready-state( p :: <document>) => (r :: <integer>)
  %ready-state(p.msxml3-interface); 
end method document-ready-state;

define method document-parse-error( p :: <document>) => (r :: <parse-error>)
  make-proxy(%parse-error(p.msxml3-interface)); 
end method document-parse-error;

define method document-url( p :: <document>) => (r :: <string>)
  convert-to-string(%url(p.msxml3-interface)); 
end method document-url;

define method document-async?( p :: <document>) => (r :: <boolean>)
  %async(p.msxml3-interface); 
end method document-async?;

define method document-async?-setter( o :: <boolean>, p :: <document> ) => (r :: <boolean>)
  p.msxml3-interface.%async := o;
end method document-async?-setter;

define method document-abort( p :: <document>) => ()
  %abort(p.msxml3-interface); 
end method document-abort;

define method document-load-xml( p :: <document>, xml :: <string>) => (r :: <boolean>)
  %load-xml(p.msxml3-interface, xml); 
end method document-load-xml;

define method document-save( p :: <document>, dest :: <object>) => (>)
  %save(p.msxml3-interface, dest); 
end method document-save;

define method document-validate-on-parse?( p :: <document>) => (r :: <boolean>)
  %validate-on-parse(p.msxml3-interface); 
end method document-validate-on-parse?;

define method document-validate-on-parse?-setter( o :: <boolean>, p :: <document> ) => (r :: <boolean>)
  p.msxml3-interface.%validate-on-parse := o;
end method document-validate-on-parse?-setter;

define method document-resolve-externals?( p :: <document>) => (r :: <boolean>)
  %resolve-externals(p.msxml3-interface); 
end method document-resolve-externals?;

define method document-resolve-externals?-setter( o :: <boolean>, p :: <document> ) => (r :: <boolean>)
  p.msxml3-interface.%resolve-externals := o;
end method document-resolve-externals?-setter;

define method document-preserve-whitespace?( p :: <document>) => (r :: <boolean>)
  %preserve-white-space(p.msxml3-interface); 
end method document-preserve-whitespace?;

define method document-preserve-whitespace?-setter( o :: <boolean>, p :: <document> ) => (r :: <boolean>)
  p.msxml3-interface.%preserve-white-space := o;
end method document-preserve-whitespace?-setter;


