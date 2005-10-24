module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <config> (<object>)
  data config-name :: <string>;
  has-many vlan;
  has-many network;
  has-many zone;
  has-many subnet;
  has-many host;
end;

define method as (class == <string>, config :: <config>)
 => (res :: <string>)
  config.config-name
end;

define method print-object (config :: <config>, stream :: <stream>)
 => ()
  format(stream, "Config:%s\n", as(<string>, config))
end;

define method fits? (network :: <network>) => (res :: <boolean>)
  fits?-aux (network, *config*.networks)
end;

define method fits? (subnet :: <subnet>) => (res :: <boolean>)
  fits?-aux(subnet, *config*.subnets)
end;

define method fits?-aux (network :: <network>, list :: <collection>)
 => (res :: <boolean>)
  //checks whether cidr is not used in network yet.
  //each network (network-address and broadcast address)
  //must be both greater than the network-address or
  //both smaller than broadcast-address
  every?(method(x)
             ((network-address(cidr(x)) > network-address(network.cidr)) &
                (broadcast-address(cidr(x)) > network-address(network.cidr))) |
             ((network-address(cidr(x)) < broadcast-address(network.cidr)) &
                (broadcast-address(cidr(x)) < broadcast-address(network.cidr)))
         end,
         list);
end;


define method add-thing (zone :: <zone>)
 => ()
  if (any?(method(x) x.zone-name = zone.zone-name end , *config*.zones))
    signal(make(<buddha-form-error>,
                error: "Zone with same name already exists, didn't add"));
  else
    *config*.zones := sort!(add!(*config*.zones, zone));
  end;
end;

define method add-thing (host :: <host>)
 => ()
  if (any?(method(x) x.host-name = host.host-name end,
           choose(method(x) x.zone = host.zone end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same name already exists in zone, didn't add"));
  elseif (any?(method(x) x.ipv4-address = host.ipv4-address end,
               choose(method(x) x.subnet = host.subnet end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same IP address already exists in subnet, didn't add"));
  elseif (any?(method(x) x.mac-address = host.mac-address end,
               choose(method(x) x.subnet = host.subnet end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same MAC address already exists in subnet, didn't add"));
  elseif ((host.ipv4-address = network-address(host.subnet.cidr)) |
            (host.ipv4-address = broadcast-address(host.subnet.cidr)))
    signal(make(<buddha-form-error>,
                error: "Host can't have the network or broadcast address as IP, didn't add"));
  else
    *config*.hosts := sort!(add!(*config*.hosts, host));
  end;
end;

define method add-thing (vlan :: <vlan>)
 => ()
  if (any?(method(x) x.number = vlan.number end , *config*.vlans))
    signal(make(<buddha-form-error>,
                error: "VLAN with same number already exists, didn't add"));
  elseif (any?(method(x) x.vlan-name = vlan.vlan-name end, *config*.vlans))
    signal(make(<buddha-form-error>,
                error: "VLAN with same name already exists, didn't add"));
  else
    *config*.vlans := sort!(add!(*config*.vlans, vlan));
  end;
end;

define method add-thing (network :: <network>)
 => ()
  unless (network-address(network.cidr) = base-network-address(network.cidr))
    signal(make(<buddha-form-error>,
                error: "Network address is not the base network address, fixing this!"));
    network.cidr.cidr-network-address := base-network-address(network.cidr);
  end;
  if (fits?(network))
    *config*.networks := sort!(add!(*config*.networks, network));
  else
    signal(make(<buddha-form-error>,
                error: "Network overlaps with another network, didn't add"));
  end if;
end;

define method add-thing (subnet :: <subnet>)
 => ()
  unless (network-address(subnet.cidr) = base-network-address(subnet.cidr))
    signal(make(<buddha-form-error>,
                error: "Network address is not the base network address, fixing this!"));
    subnet.cidr.cidr-network-address := base-network-address(subnet.cidr);
  end;
  if (fits?(subnet))
    if (subnet-in-network? (subnet))
      if (ip-in-net?(subnet, subnet.dhcp-start))
        if (ip-in-net?(subnet, subnet.dhcp-end))
          if (ip-in-net?(subnet, subnet.dhcp-router))
            *config*.subnets := sort!(add!(*config*.subnets, subnet));
          else
            signal(make(<buddha-form-error>,
                        error: "DHCP router not in subnet, didn't add"));
          end
        else
          signal(make(<buddha-form-error>,
                      error: "DHCP end not in subnet, didn't add"));
        end
      else
        signal(make(<buddha-form-error>,
                    error: "DHCP start not in subnet, didn't add"));
      end
    else
      signal(make(<buddha-form-error>,
                  error: "Subnet not in a defined network, didn't add"));
    end
  else
    signal(make(<buddha-form-error>,
                error: "Subnet overlaps with another subnet, didn't add"));
  end if;
end;

define method print-bind-zone-file
    (config :: <config>, stream :: <stream>)
 => ()
  //we need to print named.conf file here
  for (zone in config.zones)
    print-bind-zone-file(zone, stream)
  end;
end;

define method print-tinydns-zone-file
    (config :: <config>, stream :: <stream>)
 => ()
  for (zone in config.zones)
    print-tinydns-zone-file(zone, stream)
  end;
end;
