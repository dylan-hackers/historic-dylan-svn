Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <document-fragment> (<node>)
end class <document-fragment>;

define method proxy-class( interface :: <IXMLDOMDocumentFragment> ) => (class == <document-fragment>)
  <document-fragment>
end proxy-class;


define macro dom-document-fragment-methods-definer
  { define dom-document-fragment-methods ?:name ?:body end }
   => { 
        /* No Methods */
      }
end macro dom-document-fragment-methods-definer;

define dom-node-methods IXMLDOMDocumentFragment end;
define dom-document-fragment-methods IXMLDOMDocumentFragment end;


