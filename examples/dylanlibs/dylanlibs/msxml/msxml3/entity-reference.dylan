Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <entity-reference> (<node>)
end class <entity-reference>;

define method proxy-class( interface :: <IXMLDOMEntityReference> ) => (class == <entity-reference>)
  <entity-reference>
end proxy-class;

define macro dom-entity-reference-methods-definer
  { define dom-entity-reference-methods ?:name ?:body end }
   => { 
        /* No Methods */
      }
end macro dom-entity-reference-methods-definer;

define dom-node-methods IXMLDOMEntityReference end;
define dom-entity-reference-methods IXMLDOMEntityReference end;


