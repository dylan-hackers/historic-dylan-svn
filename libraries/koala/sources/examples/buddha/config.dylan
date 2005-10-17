module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <config> (<object>)
  data config-name :: <string>;
  has-many vlan;
  has-many network;
  has-many zone;
  has-many subnet;
end;

define method as (class == <string>, config :: <config>)
 => (res :: <string>)
  config.config-name
end;

define method fits? (config :: <config>, fit-cidr :: <cidr>)
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
         config.networks);
end;

define method find-network (config :: <config>, ip-address :: <ip-address>)
 => (network :: false-or(<network>))
  block(return)
    for (net in config.networks)
      if (ip-in-net?(net, ip-address))
        return(net)
      end if;
    end for;
    #f;
  end block;
end;

define method find-zone (config :: <config>, zone :: <string>)
  //XXX [0] is obviously wrong here
  choose(method(x)
             x.zone-name = zone;
         end, config.zones)[0];
end;

define method print-object (config :: <config>, stream :: <stream>)
 => ()
  format(stream, "Config: %s\n", config.config-name);
  for (net in config.networks)
    format(stream, "%=\n", net);
  end;
  for (vlan in config.vlans)
    format(stream, "%=\n", vlan);
  end for;
  for (zone in config.zones)
    format(stream, "%=\n", zone);
  end for;
end;

define method add-vlan (config :: <config>, vlan :: <vlan>)
 => ()
  if (any?(method(x) x.number = vlan.number end , config.vlans))
    format-out("VLAN %d already exists!\n", vlan.number);
  else
    config.vlans := sort!(add!(config.vlans, vlan));
  end;
end;

define method add-net (config :: <config>, network :: <network>)
 => ()
  if (fits?(*config*, network.cidr))
    config.networks := sort!(add!(config.networks, network));
  else
    format-out("Network %= overlaps with another network, not added.\n",
               network.cidr);
  end if;
end;

define method remove-vlan (config :: <config>, vlan :: <vlan>)
 => ()
  if (vlan.subnets.size = 0)
    remove!(config.vlans, vlan);
  else
    format-out("Couldn't remove vlan %d because it has subnets.\n",
               vlan.number);
  end;
end;

define method remove-net (config :: <config>, network :: <network>)
 => ()
  for (subnet in network.subnets)
    remove-subnet(network, subnet);
  end;
  config.networks := remove!(config.networks, network);
end;

define method print-bind-zone-file
    (config :: <config>, stream :: <stream>)
 => ()
  //we need to print dhcpd.conf file here
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
