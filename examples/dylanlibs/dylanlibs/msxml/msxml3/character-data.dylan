Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <character-data> (<node>)
  virtual slot character-data :: <string>;
  constant virtual slot character-data-length :: <integer>;
end class <character-data>;

define method proxy-class( interface :: <IXMLDOMCharacterData> ) => (class == <character-data>)
  <character-data>
end proxy-class;

define macro dom-character-data-methods-definer
  { define dom-character-data-methods ?:name ?:body end }
   => { 
        define proxy-method (data, ?name, ?name ## "/data") end;
        define proxy-method-setter (data-setter, ?name, ?name ## "/data") end;
        define proxy-method (length, ?name, ?name ## "/length") end;
        define proxy-method (substring-data, ?name, ?name ## "/substringData") end;
        define proxy-method (append-data, ?name, ?name ## "/appendData") end;
        define proxy-method (insert-data, ?name, ?name ## "/insertData") end;
        define proxy-method (delete-data, ?name, ?name ## "/deleteData") end;
        define proxy-method (replace-data, ?name, ?name ## "/replaceData") end;
      }
end macro dom-character-data-methods-definer;

define dom-node-methods IXMLDOMCharacterData end;
define dom-character-data-methods IXMLDOMCharacterData end;

define method character-data (p :: <character-data>) => (result :: <string>)
  convert-to-string(%data(p.msxml3-interface));
end method character-data;

define method character-data-setter( data :: <string>, p :: <character-data> ) => (s :: <string>)
  convert-to-string(p.msxml3-interface.%data := data);
end method character-data-setter;

define method character-data-length (p :: <character-data>) => (result :: <integer>)
  %length(p.msxml3-interface);
end method character-data-length;

define method character-data-substring-data (p :: <character-data>, 
    offset :: <integer>, count :: <integer>) => (r :: <string>)
  convert-to-string(%substring-data(p.msxml3-interface, offset, count));
end method character-data-substring-data;

define method character-data-append-data (p :: <character-data>, data :: <string>) => ()
  %append-data(p.msxml3-interface, data);
end method character-data-append-data;

define method character-data-insert-data (p :: <character-data>, 
    offset :: <integer>, data :: <string>) => ()
  %insert-data(p.msxml3-interface, offset, data);
end method character-data-insert-data;

define method character-data-delete-data (p :: <character-data>, 
    offset :: <integer>, count ::<integer>) => ()
  %delete-data(p.msxml3-interface, offset, count);
end method character-data-delete-data;

define method character-data-replace-data (p :: <character-data>, 
    offset :: <integer>, count :: <integer>, data :: <string>) => ()
  %replace-data(p.msxml3-interface, offset, count, data);
end method character-data-replace-data;

                           

