module: xmpp
synopsis: 
author: 
copyright:

define variable *element-translation* = make(<table>);
define class <xmpp-element> (<element>) end;
  
define generic normalize (element :: <element>);

define generic id-setter (id :: <object>, element :: <element>) => (res :: <object>);
define generic to-setter (jid :: <object>, element :: <element>) => (res :: <object>);
define generic from-setter (jid :: <object>, element :: <element>) => (res :: <object>);
define generic type-setter (type :: <object>, element :: <element>) => (res :: <object>);

define generic language-setter (language :: <object>, element :: <object>) => (res :: <object>);

define generic description-setter (description :: <object>, error :: <xmpp-error>) => (res :: <object>);
define generic condition-setter (condition :: <object>, error :: <xmpp-error>) => (res :: <object>);

define method id (element :: <element>)
 => (res :: false-or(<string>));
  let a = attribute(element, "id");
  a & a.attribute-value;
end method id;

define method id-setter (id, element :: <element>)
 => (res);
  add-attribute(element, make(<attribute>, name: "id", value: as(<string>, id)));
  id;
end method id-setter;

define method to (element :: <element>)
 => (res :: false-or(<jid>));
  let a = attribute(element, "to");
  a & as(<jid>, a.attribute-value);
end method to;

define method to-setter (jid, element :: <element>)
 => (res);
  add-attribute(element, make(<attribute>, name: "to", value: as(<string>, jid)));
  jid;
end method to-setter;

define method from (element :: <element>)
 => (res :: false-or(<jid>));
  let a = attribute(element, "from");
  a & as(<jid>, a.attribute-value);
end method from;

define method from-setter (jid, element :: <element>)
 => (res);
  add-attribute(element, make(<attribute>, name: "from", value: as(<string>, jid)));
  jid;
end method from-setter;

define method type (element :: <element>)
 => (res :: false-or(<string>));
  let a = attribute(element, "type");
  a & a.attribute-value;
end method type;

define method type-setter (type, element :: <element>)
  => (res);
  add-attribute(element, make(<attribute>, name: "type", value: as(<string>, type)));
  type;
end method type-setter;

define method language (element :: <element>)
 => (res :: false-or(<string>));
  let a = attribute(element, "xml:lang");
  a & a.attribute-value;
end method language;

define method language-setter (language, element :: <element>)
 => (res);
  add-attribute(element, make(<attribute>, name: "xml:lang", value: as(<string>, language)));
  language;
end method language-setter;

define function generate-id ()
 => (id :: <string>);
  integer-to-string(date-microseconds(current-date())); 
end function generate-id;
