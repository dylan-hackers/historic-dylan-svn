Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <attribute> (<node>)
  constant virtual slot attribute-name :: <string>;
  virtual slot attribute-value :: <object>;
end class <attribute>;

define method proxy-class( interface :: <IXMLDOMAttribute> ) => (class == <attribute>)
  <attribute>
end proxy-class;

define macro dom-attribute-methods-definer
  { define dom-attribute-methods ?:name ?:body end }
   => { 
        define proxy-method (name, ?name, ?name ## "/name") end;
        define proxy-method (value, ?name, ?name ## "/value") end;
        define proxy-method-setter (value-setter, ?name, ?name ## "/value") end;
      }
end macro dom-attribute-methods-definer;

define dom-node-methods IXMLDOMAttribute end;
define dom-attribute-methods IXMLDOMAttribute end;

define method attribute-name (attr :: <attribute>) => (s :: <string>)
  convert-to-string(%name(attr.msxml3-interface));
end method attribute-name;

define method attribute-value (attr :: <attribute>) => (r :: <object>)
  convert-to-safe-type(%value(attr.msxml3-interface));
end method attribute-value;

define method attribute-value-setter( o :: <object>, attr :: <attribute> ) => (o :: <object>)
  convert-to-safe-type(attr.msxml3-interface.%value := o);
end method attribute-value-setter;

