Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <entity> (<node>)
  constant virtual slot entity-public-id :: <object>;
  constant virtual slot entity-system-id :: <object>;
  constant virtual slot entity-notation-name :: <string>;
end class <entity>;

define method proxy-class( interface :: <IXMLDOMEntity> ) => (class == <entity>)
  <entity>
end proxy-class;

define macro dom-entity-methods-definer
  { define dom-entity-methods ?:name ?:body end }
   => { 
        define proxy-method (public-id, ?name, ?name ## "/publicId") end;
        define proxy-method (system-id, ?name, ?name ## "/systemId") end;
        define proxy-method (notation-name, ?name, ?name ## "/notationName") end;
      }
end macro dom-entity-methods-definer;

define dom-node-methods IXMLDOMEntity end;
define dom-entity-methods IXMLDOMEntity end;

define method entity-public-id (p :: <entity>) => (result :: <object>)
  convert-to-safe-type(%public-id(p.msxml3-interface));
end method entity-public-id;

define method entity-system-id (p :: <entity>) => (result :: <object>)
  convert-to-safe-type(%system-id(p.msxml3-interface));
end method entity-system-id;

define method entity-notation-name (p :: <entity>) => (result :: <object>)
  convert-to-string(%notation-name(p.msxml3-interface));
end method entity-notation-name;

