Module:    xml-converter
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define open class <xml-converter-warning> (<simple-warning>)
end class <xml-converter-warning>;

define open class <xml-converter-error> (<simple-error>, <xml-converter-warning>)
end class <xml-converter-error>;

define open class <xml-element-error> (<xml-converter-error>)
  constant slot xml-element, 
    required-init-keyword: xml-element:;
  inherited slot condition-format-arguments = make(<stretchy-vector>);
end class <xml-element-error>;

define method initialize
    (self :: <xml-element-error>, #rest args, #key xml-element) => ();
  next-method();
  add!(self.condition-format-arguments, xml-element);
end method initialize;

define class <no-conversion-for-xml-element-error> (<xml-element-error>)
  inherited slot condition-format-string = "No conversion for XML element %S.";
end class <no-conversion-for-xml-element-error>;

define class <converted-object-not-simple-error> (<xml-element-error>)
  inherited slot condition-format-string = "The XML element %S is not simple.";
end class <converted-object-not-simple-error>;

define class <xml-dylan-object-error> (<xml-element-error>)
  constant slot dylan-object, 
    required-init-keyword: dylan-object:;
  inherited slot condition-format-arguments = make(<stretchy-vector>);
end class <xml-dylan-object-error>;

define method initialize
    (self :: <xml-dylan-object-error>, #rest args, #key dylan-object) => ();
  next-method();
  add!(self.condition-format-arguments, dylan-object);
end method initialize;

define class <no-conversion-for-dylan-object-error> (<xml-converter-error>)
  inherited slot condition-format-string = "No conversion for object %S.";
end class <no-conversion-for-dylan-object-error>;

