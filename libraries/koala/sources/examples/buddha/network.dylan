module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <network> (<object>)
  data cidr :: <cidr>;
  has-many subnet;
  data dhcp? :: <boolean>;
  data dhcp-default-lease-time :: <integer>;
  data dhcp-max-lease-time :: <integer>;
  has-many dhcp-option :: <string>;
end;

//check in make or initialize (maybe of cidr???
  /*unless (network-address(cidr) = base-network-address(cidr))
    format-out("Network address is not the base network address, fixing that!\n");
    cidr.cidr-network-address := base-network-address(cidr);
  end;*/

define method \< (a :: <network>, b :: <network>)
  => (res :: <boolean>)
  a.cidr < b.cidr;
end;

define method as (class == <string>, network :: <network>)
 => (res :: <string>)
  as(<string>, network.cidr)
end;
  
define method fits? (network :: <network>, fit-cidr :: <cidr>)
 => (res :: <boolean>)
  //checks whether cidr is not used in network yet.
  //each subnet (network-address and broadcast address)
  //must be both greater than the network-address or
  //both smaller than broadcast-address
  every?(method(x)
             ((network-address(cidr(x)) > network-address(fit-cidr)) &
                (broadcast-address(cidr(x)) > network-address(fit-cidr))) |
             ((network-address(cidr(x)) < broadcast-address(fit-cidr)) &
                (broadcast-address(cidr(x)) < broadcast-address(fit-cidr)))
         end,
         network.subnets);
end;

define method find-network (network :: <network>, ip-address :: <ip-address>)
 => (subnet :: false-or(<subnet>))
  block(return)
    for (net in network.subnets)
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
  (((ip-addr > network-address(net.cidr)) |
      (ip-addr = network-address(net.cidr))) &
     (ip-addr <= broadcast-address(net.cidr)));
end;

define method subnet-in-net? (net :: <network>, subnet :: <subnet>)
 => (res :: <boolean>)
  (ip-in-net?(net, network-address(subnet.cidr)) &
     ip-in-net?(net, broadcast-address(subnet.cidr)))
end;

define method print-object (network :: <network>, stream :: <stream>)
 => ()
  format(stream, "Network: CIDR: %=\n",
         network.cidr);
  for (subnet in network.subnets)
    format(stream, "%=\n", subnet);
  end for;
end;

define method add-subnet (network :: <network>, subnet :: <subnet>)
 => ()
  if (subnet-in-net?(network, subnet))
    if (fits?(network, subnet.cidr))
      network.subnets := sort!(add!(network.subnets, subnet));
      subnet.vlan.subnets
        := sort!(add!(subnet.vlan.subnets, subnet));
      *config*.subnets := sort!(add!(*config*.subnets, subnet));
    else
      format-out("Subnet %= overlaps with another subnet, not added!\n",
                 subnet.cidr);
    end if;
  else
    format-out("Subnet %= not in network %=, not added!\n",
               subnet.cidr, network.cidr);
  end;
end;

define method remove-subnet (network :: <network>, subnet :: <subnet>)
 => ()
  network.subnets := remove!(network.subnets, subnet);
  subnet.vlan.subnets
  := remove!(subnet.vlan.subnets, subnet);
  *config*.subnets := remove!(*config*.subnets, subnet);
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
    for (subnet in network.subnets)
      print-isc-dhcpd-file(subnet, stream);
    end;
  end if;
end;
