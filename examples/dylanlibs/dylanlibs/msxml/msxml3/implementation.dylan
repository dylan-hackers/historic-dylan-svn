Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <implementation> (<msxml-proxy>)
end class <implementation>;

define method proxy-class( interface :: <IXMLDOMImplementation> ) => (class == <implementation>)
  <implementation>
end proxy-class;

define macro dom-implementation-methods-definer
  { define dom-implementation-methods ?:name ?:body end }
   => { 
        define proxy-method (has-feature, ?name, ?name ## "/hasFeature") end;
      }
end macro dom-implementation-methods-definer;

define dom-implementation-methods IXMLDOMImplementation end;

define method implementation-has-feature( p :: <implementation>, feature :: <string>,
    version :: <string>) => (r :: <boolean>)
  %has-feature(p.msxml3-interface, feature, version);
end method implementation-has-feature;
