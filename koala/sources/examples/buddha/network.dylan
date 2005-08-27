module: buddha

define class <network> (<object>)
  slot network-cidr :: <cidr>, required-init-keyword: cidr:;
  slot network-subnets :: <list> = #(), init-keyword: subnets:;
  slot network-hosts :: <list> = #(), init-keyword: hosts:;
  slot dhcp? :: <boolean> = #t, init-keyword: dhcp?:;
  slot dhcp-default-lease-time :: false-or(<integer>) = #f,
    init-keyword: dhcp-default-lease-time:;
  slot dhcp-max-lease-time :: false-or(<integer>) = #f,
    init-keyword: dhcp-max-lease-time:;
  slot dhcp-options :: <list> = #(), init-keyword: dhcp-options:;
end;

define method make (network == <network>,
                    #next next-method,
                    #rest rest,
                    #key cidr,
                    #all-keys) => (res :: <network>)
  let args = rest;
  format-out("net-make called, cidr-class %=\n",
             object-class(cidr));
  if (instance?(cidr, <string>))
    args := exclude(args, #"cidr");
    cidr := make(<cidr>,
                 network-address: cidr);
  end;
  apply(next-method, network, cidr: cidr, args);
end;

define method \< (a :: <network>, b :: <network>)
  => (res :: <boolean>)
  a.network-cidr < b.network-cidr;
end;

define method fits? (network :: <network>, cidr :: <cidr>)
 => (res :: <boolean>)
  //checks whether cidr is not used in network yet.
  //each subnet (network-address and broadcast address)
  //must be both greater than the network-address or
  //both smaller than broadcast-address
  every?(method(x)
             ((network-address(network-cidr(x)) > network-address(cidr)) &
                (broadcast-address(network-cidr(x)) > network-address(cidr))) |
             ((network-address(network-cidr(x)) < broadcast-address(cidr)) &
                (broadcast-address(network-cidr(x)) < broadcast-address(cidr)))
         end,
         network.network-subnets);
end;

define method find-network (network :: <network>, ip-address :: <ip-address>)
 => (subnet :: false-or(<subnet>))
  block(return)
    for (net in network.network-subnets)
      //format-out("FN %=\n", net);
      if (ip-in-net?(net, ip-address))
        return(net)
      end if;
    end for;
    #f;
  end block;
//  choose(method(x)
//             ip-in-subnet(x, ip-address)
//         end, network.network-subnets)[0];
end;

define method ip-in-net? (net :: <network>, ip-addr :: <ip-address>)
 => (res :: <boolean>)
  (((ip-addr > network-address(net.network-cidr)) |
      (ip-addr = network-address(net.network-cidr))) &
     (ip-addr <= broadcast-address(net.network-cidr)));
end;

define method print-object (network :: <network>, stream :: <stream>)
 => ()
  format(stream, "Network: CIDR: %=\n",
         network.network-cidr);
  for (subnet in network.network-subnets)
    format(stream, "%=\n", subnet);
  end for;
end;

define method print-html (network :: <network>, stream :: <stream>)
 => ()
  format(stream, "Network CIDR %=<br>", network.network-cidr);
  with-table (stream, #("CIDR", "VLAN"))
    do(method(x)
           print-html(x, stream);
       end, network.network-subnets);
  end;
end;

define method add-host (network :: <network>, host :: <host>)
 => ()
  network.network-hosts := sort(add(network.network-hosts, host));
  host.host-net.network-hosts
    := sort(add(host.host-net.network-hosts, host));
  host.host-zone.zone-hosts
    := sort(add(host.host-zone.zone-hosts, host));
end;

define method add-subnet (network :: <network>, subnet :: <subnet>)
 => ()
  network.network-subnets := sort(add(network.network-subnets, subnet));
  subnet.subnet-vlan.vlan-subnets
    := sort(add(subnet.subnet-vlan.vlan-subnets, subnet));
end;

define method remove-host (network :: <network>, host :: <host>)
 => ()
  network.network-hosts := remove!(network.network-hosts, host);
  host.host-net.network-hosts
    := remove!(host.host-net.network-hosts, host);
  host.host-zone.zone-hosts
    := remove!(host.host-zone.zone-hosts, host);
end;

define method remove-subnet (network :: <network>, subnet :: <subnet>)
 => ()
  network.network-subnets := remove!(network.network-subnets,
                                     subnet);
  subnet.subnet-vlan.vlan-subnets
  := remove!(subnet.subnet-vlan.vlan-subnets, subnet);
end;

define method print-isc-dhcpd-file (network :: <network>, stream :: <stream>)
  => ();
  if (network.dhcp?)
    if (network.dhcp-default-lease-time)
      format(stream, "\tdefault-lease-time %d;\n",
             network.dhcp-default-lease-time);
    end if;
    if (network.dhcp-max-lease-time)
      format(stream, "\tmax-lease-time %d;\n",
             network.dhcp-max-lease-time);
    end if;
    do(method(x)
           format(stream, "\t%s;\n", x);
       end, network.dhcp-options);
    for (subnet in network.network-subnets)
      print-isc-dhcpd-file(subnet, stream);
    end;
    for (host in network.network-hosts)
      print-isc-dhcpd-file(host, stream);
    end;
  end if;
end;
