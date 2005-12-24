module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <reference-object> (<object>)
  slot visible? :: <boolean> = #t, init-keyword: visible?:;
end;

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

define method fixup (object :: <object>)
  #t;
end;

define method check-in-context (parent :: <object>, object :: <object>)
 => (res :: <boolean>)
  check(object);
end;

define method check-in-context (tzone :: <zone>, tcname :: <cname>)
 => (res :: <boolean>)
  if (any?(method(x) x.source = tcname.source end, tzone.cnames))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  elseif (any?(method(x) x.host-name = tcname.source end, tzone.a-records))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  elseif (any?(method(x) x.host-name = tcname.source end,
               choose(method(y) y.zone = tzone end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  else
    #t;
  end;
end;

define method check-in-context (tzone :: <zone>, a-record :: <a-record>) 
 => (res :: <boolean>)
  if (any?(method(x) x.source = a-record.host-name end, tzone.cnames))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  elseif (any?(method(x) x.host-name = a-record.host-name end, tzone.a-records))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  elseif (any?(method(x) x.host-name = a-record.host-name end,
               choose(method(y) y.zone = tzone end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Same A record already exists"));
  else
    #t;
  end;
end;

define method check (zone :: <zone>)
 => (res :: <boolean>)
  if (any?(method(x) x.zone-name = zone.zone-name end , *config*.zones))
    signal(make(<buddha-form-error>,
                error: "Zone with same name already exists"));
  else
    if (zone.reverse?)
      zone.visible? := #f;
    end;
    #t;
  end;
end;

define method check (host :: <host>)
 => (res :: <boolean>)
  if (any?(method(x) x.host-name = host.host-name end,
           choose(method(x) x.zone = host.zone end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same name already exists in zone"));
  elseif (any?(method(x) x.host-name = host.host-name end,
               host.zone.a-records))
    signal(make(<buddha-form-error>,
                error: "A record for host already exists in zone"));
  elseif (any?(method(x) x.target = host.host-name end,
               host.zone.cnames))
    signal(make(<buddha-form-error>,
                error: "A record already exists in zone"));
  elseif (any?(method(x) x.ipv4-address = host.ipv4-address end,
                 choose(method(x) x.subnet = host.subnet end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same IP address already exists in subnet"));
  elseif (any?(method(x) x.mac-address = host.mac-address end,
                 choose(method(x) x.subnet = host.subnet end, *config*.hosts)))
    signal(make(<buddha-form-error>,
                error: "Host with same MAC address already exists in subnet"));
  elseif ((host.ipv4-address = network-address(host.subnet.cidr)) |
            (host.ipv4-address = broadcast-address(host.subnet.cidr)))
    signal(make(<buddha-form-error>,
                error: "Host can't have the network or broadcast address as IP"));
  elseif (~ ip-in-net?(host.subnet, host.ipv4-address))
    signal(make(<buddha-form-error>,
                error: "Host is not in specified network"))
  else
    #t;
  end;
end;

define method check (vlan :: <vlan>)
 => (res :: <boolean>)
  if ((vlan.number < 0) | (vlan.number > 4095))
    signal(make(<buddha-form-error>,
                error: "VLAN not in range 0 - 4095"));
  elseif (any?(method(x) x.number = vlan.number end , *config*.vlans))
    signal(make(<buddha-form-error>,
                error: "VLAN with same number already exists"));
  elseif (any?(method(x) x.vlan-name = vlan.vlan-name end, *config*.vlans))
    signal(make(<buddha-form-error>,
                error: "VLAN with same name already exists"));
  else
    #t;
  end;
end;

define method check (network :: <network>)
 => (res :: <boolean>)
  unless (network-address(network.cidr) = base-network-address(network.cidr))
    signal(make(<buddha-form-error>,
                error: "Network address is not the base network address, fixing this!"));
    network.cidr.cidr-network-address := base-network-address(network.cidr);
  end;
  if (fits?(network))
    if (network.reverse-dns?)
      //add reverse delegated zones...
      add-reverse-zones(network);
    end;
    #t;
  else
    signal(make(<buddha-form-error>,
                error: "Network overlaps with another network"));
  end if;
end;

define method check (subnet :: <subnet>)
 => (res :: <boolean>)
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
            if ((subnet.dhcp-router > subnet.dhcp-start)
                  & (subnet.dhcp-router < subnet.dhcp-end))
              signal(make(<buddha-form-error>,
                          error: "Router has to be outside of dhcp-range"));
            else
              #t;
            end if;
          else
            signal(make(<buddha-form-error>,
                        error: "DHCP router not in subnet"));
          end
        else
          signal(make(<buddha-form-error>,
                      error: "DHCP end not in subnet"));
        end
      else
        signal(make(<buddha-form-error>,
                    error: "DHCP start not in subnet"));
      end
    else
      signal(make(<buddha-form-error>,
                  error: "Subnet not in a defined network"));
    end
  else
    signal(make(<buddha-form-error>,
                error: "Subnet overlaps with another subnet"));
  end if;
end;

define method print-isc-dhcpd-file (config :: <config>, stream :: <stream>)
 => ()
  for (network in config.networks)
    if (network.dhcp?)
      print-isc-dhcpd-file(network, stream);
    end;
  end;
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
