module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <config> (<object>)
  constant slot config-name :: <string>, required-init-keyword: name:;
  constant slot config-vlans :: <table>, init-keyword: vlans:;
  slot config-nets :: <list> = #(), init-keyword: nets:;
  slot config-zones :: <list> = #(), init-keyword: zones:;
  //slot config-dirty? :: <boolean> = #f;
end;

define method list-type (config :: <config>, slot-name :: <string>)
  if (slot-name = "config-nets")
    as(<symbol>, "<network>")
  elseif (slot-name = "config-zones")
    as(<symbol>, "<zone>")
  end;
end;

define method make (config == <config>,
                    #next next-method,
                    #rest rest,
                    #key vlans,
                    #all-keys) => (res :: <config>)
  let args = rest;
  if (instance?(vlans, <list>))
    args := exclude(args, #"vlans");
    let vlan-table = make(<table>);
    for (vlan in vlans)
      vlan-table[vlan.vlan-number] := vlan;
    end;
    vlans := vlan-table;
  end if;
  apply(next-method, config, vlans: vlans, args);
end;

define method fits? (config :: <config>, cidr :: <cidr>)
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
         config.config-nets);
end;

define method find-network (config :: <config>, ip-address :: <ip-address>)
 => (network :: false-or(<network>))
  block(return)
    for (net in config.config-nets)
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

define method find-zone (config :: <config>, zone :: <string>)
  choose(method(x)
             x.zone-name = zone;
         end, config.config-zones)[0];
end;

define method print-object (config :: <config>, stream :: <stream>)
 => ()
  format(stream, "Config: %s\n", config.config-name);
  for (net in config.config-nets)
    format(stream, "%=\n", net);
  end;
  for (vlan in config.config-vlans)
    format(stream, "%=\n", vlan);
  end for;
  for (zone in config.config-zones)
    format(stream, "%=\n", zone);
  end for;
end;

define method add-vlan (config :: <config>, vlan :: <vlan>)
 => ()
  if (element(config.config-vlans, vlan.vlan-number, default: #f))
    format-out("VLAN %d already exists!\n", vlan.vlan-number);
  else
    config.config-vlans[vlan.vlan-number] := vlan;
  end;
end;

define method add-net (config :: <config>, network :: <network>)
 => ()
  if (fits?(*config*, network.network-cidr))
    config.config-nets := sort!(add!(config.config-nets, network));
  else
    format-out("Network %= overlaps with another network, not added.\n",
               network.network-cidr);
  end if;
end;

define method remove-vlan (config :: <config>, vlan-number :: <integer>)
 => ()
  let vlan = config.config-vlans[vlan-number];
  if (vlan.vlan-subnets.size = 0)
    remove-key!(config.config-vlans, vlan-number);
  else
    format-out("Couldn't remove vlan %d because it has subnets.\n",
               vlan-number);
  end;
end;

define method remove-net (config :: <config>, network :: <network>)
 => ()
  for (subnet in network.network-subnets)
    remove-subnet(network, subnet);
  end;
  config.config-nets := remove!(config.config-nets, network);
end;

define method print-bind-zone-file
    (config :: <config>, stream :: <stream>)
 => ()
  //we need to print dhcpd.conf file here
  for (zone in config.config-zones)
    print-bind-zone-file(zone, stream)
  end;
end;

define method print-tinydns-zone-file
    (config :: <config>, stream :: <stream>)
 => ()
  for (zone in config.config-zones)
    print-tinydns-zone-file(zone, stream)
  end;
end;
