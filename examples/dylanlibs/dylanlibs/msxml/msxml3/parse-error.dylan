Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <parse-error> (<msxml-proxy>)
  constant virtual slot parse-error-code :: <integer>;
  constant virtual slot parse-error-url :: <string>;
  constant virtual slot parse-error-reason :: <string>;
  constant virtual slot parse-error-src-text :: <string>;
  constant virtual slot parse-error-line :: <integer>;
  constant virtual slot parse-error-line-pos :: <integer>;
  constant virtual slot parse-error-file-pos :: <integer>;
end class <parse-error>;

define method proxy-class( interface :: <IXMLDOMParseError> ) => (class == <parse-error>)
  <parse-error>
end proxy-class;

define macro dom-parse-error-methods-definer
  { define dom-parse-error-methods ?:name ?:body end }
   => { 
        define proxy-method (error-code, ?name, ?name ## "/errorCode") end;
        define proxy-method (url, ?name, ?name ## "/url") end;
        define proxy-method (reason, ?name, ?name ## "/reason") end;
        define proxy-method (src-text, ?name, ?name ## "/srcText") end;
        define proxy-method (line, ?name, ?name ## "/line") end;
        define proxy-method (line-pos, ?name, ?name ## "/linePos") end;
        define proxy-method (file-pos, ?name, ?name ## "/filePos") end;
      }
end macro dom-parse-error-methods-definer;

define dom-parse-error-methods IXMLDOMParseError end;

define method parse-error-code (p :: <parse-error>) => (result :: <integer>)
  %error-code(p.msxml3-interface);
end method parse-error-code;

define method parse-error-url (p :: <parse-error>) => (result :: <string>)
  convert-to-string(%url(p.msxml3-interface));
end method parse-error-url;

define method parse-error-reason (p :: <parse-error>) => (result :: <string>)
  convert-to-string(%reason(p.msxml3-interface));
end method parse-error-reason;

define method parse-error-src-text (p :: <parse-error>) => (result :: <string>)
  convert-to-string(%src-text(p.msxml3-interface));
end method parse-error-src-text;

define method parse-error-line (p :: <parse-error>) => (result :: <integer>)
  %line(p.msxml3-interface);
end method parse-error-line;

define method parse-error-line-pos (p :: <parse-error>) => (result :: <integer>)
  %line-pos(p.msxml3-interface);
end method parse-error-line-pos;

define method parse-error-file-pos (p :: <parse-error>) => (result :: <integer>)
  %file-pos(p.msxml3-interface);
end method parse-error-file-pos;


