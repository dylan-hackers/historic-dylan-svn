Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <processing-instruction> (<node>)
  constant virtual slot processing-instruction-target :: <string>;
  virtual slot processing-instruction-data :: <string>;
end class <processing-instruction>;

define method proxy-class( interface :: <IXMLDOMProcessingInstruction> ) => (class == <processing-instruction>)
  <processing-instruction>
end proxy-class;

define macro dom-processing-instruction-methods-definer
  { define dom-processing-instruction-methods ?:name ?:body end }
   => { 
        define proxy-method (target, ?name, ?name ## "/target") end;
        define proxy-method (data, ?name, ?name ## "/data") end;
        define proxy-method-setter (data-setter, ?name, ?name ## "/data") end;
      }
end macro dom-processing-instruction-methods-definer;

define dom-node-methods IXMLDOMProcessingInstruction end;
define dom-processing-instruction-methods IXMLDOMProcessingInstruction end;

define method processing-instruction-target (p :: <processing-instruction>) => (result :: <string>)
  convert-to-string(%target(p.msxml3-interface));
end method processing-instruction-target;

define method processing-instruction-data (p :: <processing-instruction>) => (result :: <string>)
  convert-to-string(%data(p.msxml3-interface));
end method processing-instruction-data;

define method processing-instruction-data-setter( data :: <string>, p :: <processing-instruction> ) => (s :: <string>)
  convert-to-string(p.msxml3-interface.%data := data);
end method processing-instruction-data-setter;


