Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <text> (<character-data>)
end class <text>;

define method proxy-class( interface :: <IXMLDOMText> ) => (class == <text>)
  <text>
end proxy-class;

define macro dom-text-methods-definer
  { define dom-text-methods ?:name ?:body end }
   => { 
        define proxy-method (split-text, ?name, ?name ## "/splitText") end;
      }
end macro dom-text-methods-definer;

define dom-node-methods IXMLDOMText end;
define dom-character-data-methods IXMLDOMText end;
define dom-text-methods IXMLDOMText end;

define method text-split-text (p :: <text>, offset :: <integer>) => (n :: <text>)
  make-proxy(%split-text(p.msxml3-interface, offset));
end method text-split-text;

                           

