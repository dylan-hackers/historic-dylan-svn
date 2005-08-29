module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <config> (<object>)
  slot config-name :: <string>, required-init-keyword: name:;
  slot config-vlans :: <table>, init-keyword: vlans:;
  slot config-nets :: <list> = #(), init-keyword: nets:;
  slot config-zones :: <list> = #(), init-keyword: zones:;
  slot config-dirty? :: <boolean> = #f;
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
  config.config-vlans[vlan.vlan-number] := vlan;
end;

define method add-net (config :: <config>, network :: <network>)
 => ()
  config.config-nets := sort(add(config.config-nets, network));
end;

define method remove-vlan (config :: <config>, vlan :: <vlan>)
 => ()
  config.config-vlans := remove!(config.config-vlans, vlan);
end;

define method remove-net (config :: <config>, network :: <network>)
 => ()
  config.config-nets := remove!(config.config-nets, network);
end;
