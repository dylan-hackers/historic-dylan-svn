module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <vlan> (<object>)
  slot vlan-number :: <integer>, required-init-keyword: number:;
  slot vlan-name :: <string>, init-keyword: name:;
  slot vlan-description :: <string>, init-keyword: description:;
  slot vlan-subnets :: <list> = #();
end;

define method print-object (vlan :: <vlan>, stream :: <stream>)
 => ()
  format(stream, "VLAN %d name %s description %s",
        vlan.vlan-number, vlan.vlan-name, vlan.vlan-description);
end;

define method gen-xml (vlan :: <vlan>)
  let res = make(<list>);
  res := add!(res, with-xml()
                     text(concatenate("VLAN ",
                                      integer-to-string(vlan.vlan-number), " ",
                                      vlan.vlan-name, " ",
                                      vlan.vlan-description))
                   end);
  res := add!(res, with-xml()
                     table
                     {
                       tr { th("CIDR"), th("VLAN") },
                       do(let res = make(<list>);
                          do(method(x)
                                 res := add!(res, gen-xml(x));
                             end,
                             vlan.vlan-subnets);
                          reverse(res))
                     }
                   end);
  res;
end;

define method as (class == <string>, vlan :: <vlan>)
 => (res :: <string>)
  concatenate(integer-to-string(vlan.vlan-number), " ", vlan.vlan-name);
end;

define method \< (a :: <vlan>, b :: <vlan>)
 => (res :: <boolean>)
  a.vlan-number < b.vlan-number
end;
