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

define method print-html (vlan :: <vlan>, stream :: <stream>)
  => ()
  format(stream, "VLAN %d %s %s<br/>\n",
         vlan.vlan-number,
         vlan.vlan-name,
         vlan.vlan-description);
  with-table (stream, #("CIDR", "VLAN"))
    do(method(x) print-html(x, stream); end,
       vlan.vlan-subnets);
  end;
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
                       do(do(method(x) gen-xml(x); end,
                             vlan.vlan-subnets))
                     }
                   end);
  res;
end;
              
define method \< (a :: <vlan>, b :: <vlan>)
  => (res :: <boolean>)
  a.vlan-number < b.vlan-number
end;
