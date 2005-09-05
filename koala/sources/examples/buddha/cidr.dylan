module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <cidr> (<object>)
  slot cidr-network-address :: <ip-address>,
    required-init-keyword: network-address:;
  slot cidr-netmask :: <integer>,
    required-init-keyword: netmask:;
end class;

define method make (cidr == <cidr>,
                    #next next-method,
                    #rest rest,
                    #key network-address,
                    netmask,
                    #all-keys) => (res :: <cidr>)
  let args = rest;
  if (instance?(network-address, <string>))
    args := exclude(args, #"network-address");
    //support for xx.xx.xx.xx/yy
    if (any?(method(x) x = '/' end, network-address))
      let address-and-mask = split(network-address, '/');
      network-address := address-and-mask[0];
      netmask := address-and-mask[1];
    end if;
    network-address := make(<ip-address>, ip: network-address);
  end;
  if (instance?(netmask, <string>))
    if (any?(method(x) x = '.' end, netmask))
      //support for xx.xx.xx.xx
      netmask := string-to-netmask(netmask);
    else
      netmask := string-to-integer(netmask);
    end if;
  end if;
  apply(next-method, cidr, network-address: network-address,
        netmask: netmask, args);
end;

define method \< (a :: <cidr>, b :: <cidr>)
 => (res :: <boolean>)
  a.cidr-network-address < b.cidr-network-address
end;

define method print-object (cidr :: <cidr>, stream :: <stream>)
 => ()
  format(stream, "%s", as(<string>, cidr));
end;

define method as (class == <string>, cidr :: <cidr>)
 => (res :: <string>)
  concatenate(as(<string>, network-address(cidr)), "/",
              integer-to-string(cidr.cidr-netmask));
end;

define method base-network-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  make(<ip-address>,
       ip: map(logand,
               ip(netmask-address(cidr)),
               ip(network-address(cidr))));
end;

define method broadcast-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  let mask = map(method(x)
                     logand(255, lognot(x));
                 end, ip(netmask-address(cidr)));
  make(<ip-address>,
       ip: map(logior,
               ip(network-address(cidr)),
               mask));
end;

define method network-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  cidr.cidr-network-address;
end;

define method netmask-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  as(<ip-address>, cidr.cidr-netmask);
end;
