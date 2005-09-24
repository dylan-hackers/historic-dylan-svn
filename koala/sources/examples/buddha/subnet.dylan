module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <subnet> (<network>)
  slot subnet-vlan :: false-or(<vlan>) = #f, init-keyword: vlan:;
  slot subnet-hosts :: <list> = #(), init-keyword: hosts:;
  slot dhcp-start :: <ip-address>, init-keyword: dhcp-start:;
  slot dhcp-end :: <ip-address>, init-keyword: dhcp-end:;
  slot dhcp-router :: false-or(<ip-address>) = #f,
    init-keyword: dhcp-router:;
end;

define method list-type(subnet :: <subnet>, slot-name :: <string>)
  if (slot-name = "subnet-hosts")
    as(<symbol>, "<host>")
  elseif (slot-name = "dhcp-options")
    as(<symbol>, "<string>")
  end;
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
  unless (dhcp-router)
    args := exclude(args, #"dhcp-router");
    dhcp-router := network-address(cidr) + 1;
  end;
  unless (network-address(cidr) = base-network-address(cidr))
    format-out("Network address is not the base network address, fixing this!\n");
    cidr.cidr-network-address := base-network-address(cidr);
  end;
  apply(next-method, subnet, cidr: cidr, vlan: vlan,
        dhcp-start: dhcp-start, dhcp-end: dhcp-end,
        dhcp-router: dhcp-router, args);
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

define method as (class == <string>, subnet :: <subnet>)
 => (res :: <string>)
  as(<string>, subnet.network-cidr);
end;

define method gen-xml (subnet :: <subnet>)
  with-xml()
    tr { td(as(<string>, subnet.network-cidr)),
         td(integer-to-string(subnet.subnet-vlan.vlan-number))
       }
  end;
end;

define method add-host (subnet :: <subnet>, host :: <host>)
 => ()
  if ((host.host-ipv4-address = network-address(subnet.network-cidr)) |
        (host.host-ipv4-address = broadcast-address(subnet.network-cidr)))
    format-out("Host can't have the network or broadcast address as IP %=\n",
               host);
  elseif (member?(host,
              subnet.subnet-hosts,
              test: method(x, y)
                        x.host-ipv4-address = y.host-ipv4-address;
                    end))
    format-out("Host with same IP already exists: %=\n", host);
  elseif (member?(host,
                  host.host-zone.zone-hosts,
                  test: method(x, y)
                            x.host-name = y.host-name
                        end))
    format-out("Host with same name already exists: %=\n", host);
  elseif (member?(host,
                  subnet.subnet-hosts,
                  test: method(x, y)
                            x.host-mac = y.host-mac
                        end))
    format-out("Host with same mac already exists in this subnet: %=\n", host);
  else
    subnet.subnet-hosts := sort!(add!(subnet.subnet-hosts, host));
    host.host-zone.zone-hosts
      := sort!(add!(host.host-zone.zone-hosts, host));
  end;
end;

define method remove-host (subnet :: <subnet>, host :: <host>)
 => ()
  subnet.subnet-hosts := remove!(subnet.subnet-hosts, host);
  host.host-zone.zone-hosts
    := remove!(host.host-zone.zone-hosts, host);
end;


define method print-isc-dhcpd-file (subnet :: <subnet>, stream :: <stream>)
 => ()
  if (subnet.dhcp?)
    format(stream, "subnet %s netmask %s {\n",
           as(<string>, network-address(subnet.network-cidr)),
           as(<string>, netmask-address(subnet.network-cidr)));
    if (subnet.dhcp-router)
      format(stream, "\toption routers %s;\n",
             as(<string>, subnet.dhcp-router));
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
                  as(<string>, head(x)),
                  as(<string>, tail(x)));
       end, generate-dhcp-ranges(subnet));
    format(stream, "}\n\n");
    for (host in subnet.subnet-hosts)
      print-isc-dhcpd-file(host, stream);
    end;
  end if;
end;

define method generate-dhcp-ranges (subnet :: <subnet>)
 => (list :: <list>)
  let start-ip :: <ip-address> = subnet.dhcp-start;
  let end-ip :: <ip-address> = subnet.dhcp-end;
  let res = make(<list>);
  for (host in subnet.subnet-hosts)
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
