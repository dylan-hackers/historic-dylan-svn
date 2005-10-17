module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

//XXX: this should be dynamic generated...
//without these I get lots of warnings:
//Invalid type for argument object in call to
// hosts (object :: <object>) => (#rest results :: <object>)
// :  <zone> supplied, <subnet> expected.
define dynamic generic hosts (o :: <object>) => (r :: <object>);
define dynamic generic hosts-setter (h :: <object>, o :: <object>)
 => (r :: <object>);

define web-class <subnet> (<network>)
  has-a vlan;
  has-many host;
  data dhcp-start :: <ip-address>;
  data dhcp-end :: <ip-address>;
  data dhcp-router :: <ip-address>;
end;

/* chech in make or initialize or before all that stuff
  unless (network-address(cidr) = base-network-address(cidr))
    format-out("Network address is not the base network address, fixing this!\n");
    cidr.cidr-network-address := base-network-address(cidr);
  end;
dhcp-start in subnet, dhcp-end in subnet, dhcp-router nicht in dhcp-range und in subnet
*/

define method print-object (subnet :: <subnet>, stream :: <stream>)
 => ()
  if (subnet.vlan)
    format(stream, "Subnet vlan %d cidr %=",
           subnet.vlan.number,
           subnet.cidr);
  else
    format(stream, "Subnet cidr %=",
           subnet.cidr);
  end;
end;

define method as (class == <string>, subnet :: <subnet>)
 => (res :: <string>)
  as(<string>, subnet.cidr);
end;

define method add-host (subnet :: <subnet>, host :: <host>)
 => ()
  if ((host.ipv4-address = network-address(subnet.cidr)) |
        (host.ipv4-address = broadcast-address(subnet.cidr)))
    format-out("Host can't have the network or broadcast address as IP %=\n",
               host);
  elseif (member?(host,
              subnet.hosts,
              test: method(x, y)
                        x.ipv4-address = y.ipv4-address;
                    end))
    format-out("Host with same IP already exists: %=\n", host);
  elseif (member?(host,
                  host.zone.hosts,
                  test: method(x, y)
                            x.host-name = y.host-name
                        end))
    format-out("Host with same name already exists: %=\n", host);
  elseif (member?(host,
                  subnet.hosts,
                  test: method(x, y)
                            x.mac-address = y.mac-address
                        end))
    format-out("Host with same mac already exists in this subnet: %=\n", host);
  else
    subnet.hosts := sort!(add!(subnet.hosts, host));
    host.zone.hosts
      := sort!(add!(host.zone.hosts, host));
  end;
end;

define method remove-host (subnet :: <subnet>, host :: <host>)
 => ()
  subnet.hosts := remove!(subnet.hosts, host);
  host.zone.hosts
    := remove!(host.zone.hosts, host);
end;


define method print-isc-dhcpd-file (subnet :: <subnet>, stream :: <stream>)
 => ()
  if (subnet.dhcp?)
    format(stream, "subnet %s netmask %s {\n",
           as(<string>, network-address(subnet.cidr)),
           as(<string>, netmask-address(subnet.cidr)));
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
    for (host in subnet.hosts)
      print-isc-dhcpd-file(host, stream);
    end;
  end if;
end;

define method generate-dhcp-ranges (subnet :: <subnet>)
 => (list :: <list>)
  let start-ip :: <ip-address> = subnet.dhcp-start;
  let end-ip :: <ip-address> = subnet.dhcp-end;
  let res = make(<list>);
  for (host in subnet.hosts)
    let host-ip = host.ipv4-address;
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
