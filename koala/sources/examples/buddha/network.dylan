module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <network> (<reference-object>)
  data cidr :: <cidr>;
  data dhcp? :: <boolean> = #t;
  data dhcp-default-lease-time :: <integer> = 600;
  data dhcp-max-lease-time :: <integer> = 7200;
  slot reverse-dns? :: <boolean>;
  has-many dhcp-option :: <string>;
end;

define method \< (a :: <network>, b :: <network>)
 => (res :: <boolean>)
  a.cidr < b.cidr;
end;

define method as (class == <string>, network :: <network>)
 => (res :: <string>)
  as(<string>, network.cidr)
end;

define method subnet-in-network? (subnet :: <subnet>)
 => (res :: <boolean>)
  //we already know that the subnet doesn't conflict with other subnets
  //and only need to check whether it is in the network subnet.network
  let sub-cidr = subnet.cidr;
  let net-cidr = subnet.network.cidr;
  if (((network-address(sub-cidr) > network-address(net-cidr)) |
         (network-address(sub-cidr) = network-address(net-cidr))) &
        ((broadcast-address(sub-cidr) < broadcast-address(net-cidr)) |
           (broadcast-address(sub-cidr) = broadcast-address(net-cidr))))
    #t
  else
    #f
  end
end;

define method ip-in-net? (net :: <network>, ip-addr :: <ip-address>)
 => (res :: <boolean>)
  (((ip-addr > network-address(net.cidr)) |
      (ip-addr = network-address(net.cidr))) &
     (ip-addr <= broadcast-address(net.cidr)));
end;

define method print-object (network :: <network>, stream :: <stream>)
 => ()
  format(stream, "Network: CIDR: %s\n", as(<string>, network));
end;

define method print-isc-dhcpd-file (print-network :: <network>,
                                    stream :: <stream>)
  => ();
  if (print-network.dhcp?)
    if (print-network.dhcp-default-lease-time)
      format(stream, "default-lease-time %d;\n",
             print-network.dhcp-default-lease-time);
    end if;
    if (print-network.dhcp-max-lease-time)
      format(stream, "max-lease-time %d;\n",
             print-network.dhcp-max-lease-time);
    end if;
    do(method(x)
           format(stream, "%s;\n", x);
       end, print-network.dhcp-options);
    format(stream, "\n");
    do(method(x)
           print-isc-dhcpd-file(x, stream);
       end, choose(method(x)
                       x.network = print-network
                   end, *config*.subnets))
  end if;
end;
