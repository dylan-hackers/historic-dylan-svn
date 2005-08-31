module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <subnet> (<network>)
  slot subnet-vlan :: false-or(<vlan>) = #f, init-keyword: vlan:;
  slot dhcp-start :: <ip-address>, init-keyword: dhcp-start:;
  slot dhcp-end :: <ip-address>, init-keyword: dhcp-end:;
  slot dhcp-router :: false-or(<ip-address>) = #f,
    init-keyword: dhcp-router:;
end;

define method make (subnet == <subnet>,
                    #next next-method,
                    #rest rest,
                    #key cidr,
                    dhcp-start,
                    dhcp-end,
                    dhcp-router,
                    vlan,
                    #all-keys) => (res :: <subnet>)
  let args = rest;
  if (instance?(cidr, <string>))
    args := exclude(args, #"cidr");
    cidr := make(<cidr>,
                 network-address: cidr);
  end if;
  if (instance?(vlan, <integer>))
    args := exclude(args, #"vlan");
    vlan := *config*.config-vlans[vlan];
  end;
  unless (dhcp-start)
    args := exclude(args, #"dhcp-start");
    dhcp-start := network-address(cidr) + 1;
  end unless;
  unless (dhcp-end)
    args := exclude(args, #"dhcp-end");
    dhcp-end := broadcast-address(cidr) - 1;
  end unless;
  apply(next-method, subnet, cidr: cidr, vlan: vlan,
        dhcp-start: dhcp-start, dhcp-end: dhcp-end, args);
end;

define method print-object (subnet :: <subnet>, stream :: <stream>)
 => ()
  if (subnet.subnet-vlan)
    format(stream, "Subnet vlan %d cidr %=",
           subnet.subnet-vlan.vlan-number,
           subnet.network-cidr);
  else
    format(stream, "Subnet cidr %=",
           subnet.network-cidr);
  end;
end;

define method print-html (subnet :: <subnet>, stream :: <stream>)
 => ()
  //gen-row(stream, #(subnet.network-cidr, subnet.subnet-vlan.vlan-number))
  format(stream, "<tr>");
  print-html(subnet.network-cidr, stream);
  format(stream, "<td>%d</td></tr>", subnet.subnet-vlan.vlan-number);
end;

define method print-isc-dhcpd-file (subnet :: <subnet>, stream :: <stream>)
 => ()
  if (subnet.dhcp?)
    format(stream, "subnet %s netmask %s {\n",
           ip-address-to-string(network-address(subnet.network-cidr)),
           ip-address-to-string(netmask-address(subnet.network-cidr)));
    if (subnet.dhcp-router)
      format(stream, "\toption routers %s;\n",
             ip-address-to-string(subnet.dhcp-router));
    end if;
    if (subnet.dhcp-default-lease-time)
      format(stream, "\tdefault-lease-time %d;\n",
             subnet.dhcp-default-lease-time);
    end if;
    if (subnet.dhcp-max-lease-time)
      format(stream, "\tmax-lease-time %d;\n",
             subnet.dhcp-max-lease-time);
    end if;
    do(method(x)
           format(stream, "\t%s;\n", x);
       end, subnet.dhcp-options);
    do(method(x)
           format(stream, "\trange %s %s;\n",
                  ip-address-to-string(head(x)),
                  ip-address-to-string(tail(x)));
       end, generate-dhcp-ranges(subnet));
    format(stream, "}\n\n");
  end if;
end;

define method generate-dhcp-ranges (subnet :: <subnet>)
 => (list :: <list>)
  let start-ip :: <ip-address> = subnet.dhcp-start;
  let end-ip :: <ip-address> = subnet.dhcp-end;
  let res = make(<list>);
  for (host in subnet.network-hosts)
    let host-ip = host.host-ipv4-address;
    if ((host-ip > start-ip) & (host-ip < end-ip))
      res := add!(res, pair(start-ip, host-ip - 1));
    end;
    if (host-ip >= start-ip)
      start-ip := host-ip + 1;
    end;
  end for;
  if (start-ip <= end-ip)
    res := add!(res, pair(start-ip, end-ip));
  end;
  //format-out("ranges %=\n", reverse(res));
  reverse(res);
end;
