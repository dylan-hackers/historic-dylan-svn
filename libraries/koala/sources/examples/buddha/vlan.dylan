module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <vlan> (<object>)
  data number :: <integer>;
  data vlan-name :: <string>;
  data description :: <string>;
  has-many subnet;
end;

define method print-object (vlan :: <vlan>, stream :: <stream>)
 => ()
  format(stream, "VLAN %d name %s description %s",
        vlan.number, vlan.vlan-name, vlan.description);
end;

define method gen-xml (vlan :: <vlan>)
  let res = make(<list>);
  res := add!(res, with-xml()
                     text(concatenate("VLAN ",
                                      integer-to-string(vlan.number), " ",
                                      vlan.vlan-name, " ",
                                      vlan.description))
                   end);
  res := add!(res, with-xml()
                     table
                     {
                       tr { th("CIDR"), th("VLAN") },
                       do(let res = make(<list>);
                          do(method(x)
                                 res := add!(res, gen-xml(x));
                             end,
                             vlan.subnets);
                          reverse(res))
                     }
                   end);
  res;
end;

define method as (class == <string>, vlan :: <vlan>)
 => (res :: <string>)
  concatenate(integer-to-string(vlan.number), " ", vlan.vlan-name);
end;

define method \< (a :: <vlan>, b :: <vlan>)
 => (res :: <boolean>)
  a.number < b.number
end;
