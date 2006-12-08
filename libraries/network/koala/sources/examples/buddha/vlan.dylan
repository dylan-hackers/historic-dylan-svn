module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <vlan> (<reference-object>)
  data vlan-number :: <integer>;
  data vlan-name :: <string>;
  data vlan-description :: <string>;
end;

define method print-object (vlan :: <vlan>, stream :: <stream>)
 => ()
  format(stream, "VLAN %s\n", as(<string>, vlan))
end;

define method as (class == <string>, vlan :: <vlan>)
 => (res :: <string>)
  concatenate(integer-to-string(vlan.vlan-number), " ", vlan.vlan-name);
end;

define method \< (a :: <vlan>, b :: <vlan>)
 => (res :: <boolean>)
  a.vlan-number < b.vlan-number
end;
