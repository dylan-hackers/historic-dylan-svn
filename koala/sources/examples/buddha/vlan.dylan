module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <vlan> (<object>)
  data number :: <integer>;
  data vlan-name :: <string>;
  data description :: <string>;
end;

define method print-object (vlan :: <vlan>, stream :: <stream>)
 => ()
  format(stream, "VLAN %s\n", as(<string>, vlan))
end;

define method as (class == <string>, vlan :: <vlan>)
 => (res :: <string>)
  concatenate(integer-to-string(vlan.number), " ", vlan.vlan-name);
end;

define method \< (a :: <vlan>, b :: <vlan>)
 => (res :: <boolean>)
  a.number < b.number
end;
