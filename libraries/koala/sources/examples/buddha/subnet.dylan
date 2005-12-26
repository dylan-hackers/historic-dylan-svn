module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

//XXX: this should be dynamic generated...
//without these I get lots of warnings:
//Invalid type for argument object in call to
// hosts (object :: <object>) => (#rest results :: <object>):  <zone> supplied, <subnet> expected.
//define dynamic generic hosts (o :: <object>) => (r :: <object>);
//define dynamic generic hosts-setter (h :: <object>, o :: <object>)
// => (r :: <object>);

define web-class <subnet> (<network>)
  has-a vlan;
  has-a network;
  data dhcp-start :: <ip-address>, base-network-address(object.cidr) + 21;
  data dhcp-end :: <ip-address>, broadcast-address(object.cidr) - 1;
  data dhcp-router :: <ip-address>, base-network-address(object.cidr) + 1;
end;

define method print-object (subnet :: <subnet>, stream :: <stream>)
 => ()
  format(stream, "Subnet %s\n", as(<string>, subnet));
end;

define method as (class == <string>, subnet :: <subnet>)
 => (res :: <string>)
  as(<string>, subnet.cidr);
end;

define method print-isc-dhcpd-file (print-subnet :: <subnet>, stream :: <stream>)
 => ()
  if (print-subnet.dhcp?)
    format(stream, "subnet %s netmask %s {\n",
           as(<string>, network-address(print-subnet.cidr)),
           as(<string>, netmask-address(print-subnet.cidr)));
    if (print-subnet.dhcp-router)
      format(stream, "\toption routers %s;\n",
             as(<string>, print-subnet.dhcp-router));
    end if;
    if (print-subnet.dhcp-default-lease-time)
      format(stream, "\tdefault-lease-time %d;\n",
             print-subnet.dhcp-default-lease-time);
    end if;
    if (print-subnet.dhcp-max-lease-time)
      format(stream, "\tmax-lease-time %d;\n",
             print-subnet.dhcp-max-lease-time);
    end if;
    do(method(x)
           format(stream, "\t%s\n", x);
       end, print-subnet.dhcp-options);
    do(method(x)
           format(stream, "\trange %s %s;\n",
                  as(<string>, head(x)),
                  as(<string>, tail(x)));
       end, generate-dhcp-ranges(print-subnet));
    format(stream, "}\n\n");
    do(method(x)
           print-isc-dhcpd-file(x, stream);
       end, choose(method(x)
                       x.subnet = print-subnet
                   end, *config*.hosts))

  end if;
end;

define method generate-dhcp-ranges (this-subnet :: <subnet>)
 => (list :: <list>)
  let start-ip :: <ip-address> = this-subnet.dhcp-start;
  let end-ip :: <ip-address> = this-subnet.dhcp-end;
  let res = make(<list>);
  for (host in choose(method(x)
                          x.subnet = this-subnet
                      end, *config*.hosts))
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
  reverse(res);
end;
