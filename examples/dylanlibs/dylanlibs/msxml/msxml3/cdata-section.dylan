Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <cdata-section> (<text>)
end class <cdata-section>;

define method proxy-class( interface :: <IXMLDOMCDATASection> ) => (class == <cdata-section>)
  <cdata-section>
end proxy-class;

define macro dom-cdata-section-methods-definer
  { define dom-cdata-section-methods ?:name ?:body end }
   => { 
        /* No Methods */
      }
end macro dom-cdata-section-methods-definer;

define dom-node-methods IXMLDOMCDATASection end;
define dom-character-data-methods IXMLDOMCDATASection end;
define dom-text-methods IXMLDOMCDATASection end;
define dom-cdata-section-methods IXMLDOMCDATASection end;

