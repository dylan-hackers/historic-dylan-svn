Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <notation> (<node>)
  constant virtual slot notation-public-id :: <object>;
  constant virtual slot notation-system-id :: <object>;
end class <notation>;

define method proxy-class( interface :: <IXMLDOMNotation> ) => (class == <notation>)
  <notation>
end proxy-class;

define macro dom-notation-methods-definer
  { define dom-notation-methods ?:name ?:body end }
   => { 
        define proxy-method (public-id, ?name, ?name ## "/publicId") end;
        define proxy-method (system-id, ?name, ?name ## "/systemId") end;
      }
end macro dom-notation-methods-definer;

define dom-node-methods IXMLDOMNotation end;
define dom-notation-methods IXMLDOMNotation end;

define method notation-public-id (p :: <notation>) => (result :: <object>)
  convert-to-safe-type(%public-id(p.msxml3-interface));
end method notation-public-id;

define method notation-system-id (p :: <notation>) => (result :: <object>)
  convert-to-safe-type(%system-id(p.msxml3-interface));
end method notation-system-id;


