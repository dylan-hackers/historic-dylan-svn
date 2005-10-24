module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <cidr> (<object>)
  slot cidr-network-address :: <ip-address>,
    required-init-keyword: network-address:;
  slot cidr-netmask :: <integer>,
    required-init-keyword: netmask:;
end class;

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

define method as(class == <cidr>, string :: <string>)
 => (res :: <cidr>)
  let address-and-mask = split(string, '/');
  let network-address = address-and-mask[0];
  let netmask = address-and-mask[1];
  network-address := make(<ip-address>, data: network-address);
  if (any?(method(x) x = '.' end, netmask))
    //support for xx.xx.xx.xx
    netmask := string-to-netmask(netmask);
  else
    netmask := string-to-integer(netmask);
  end if;
  make(<cidr>, network-address: network-address, netmask: netmask)
end;

define method base-network-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  make(<ip-address>,
       data: map(logand,
                 netmask-address(cidr),
                 network-address(cidr)))
end;

define method broadcast-address (cidr :: <cidr>)
 => (ip-address :: <ip-address>)
  let mask = map(method(x)
                     logand(255, lognot(x));
                 end, netmask-address(cidr));
  make(<ip-address>,
       data: map(logior,
                 network-address(cidr),
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
