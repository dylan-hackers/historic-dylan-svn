module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <cname> (<object>)
  data source :: <string>;
  data target :: <string>;
end;

define method print-object (cname :: <cname>, stream :: <stream>)
 => ();
  format(stream, "CNAME: %s\n", as(<string>, cname));
end method;

define method as (class == <string>, cname :: <cname>)
 => (res :: <string>)
  concatenate(cname.source, " => ", cname.target);
end;

define method \< (a :: <cname>, b :: <cname>)
 => (res :: <boolean>)
  a.source < b.source
end;

define web-class <a-record> (<object>)
  data host-name :: <string>;
  data ipv4-address :: <ip-address>;
  data time-to-live :: <integer> = 300;
end;

define method print-object (a :: <a-record>, stream :: <stream>)
 => ();
  format(stream, "A: %s\n", as(<string>, a));
end method;

define method as (class == <string>, a :: <a-record>)
 => (res :: <string>)
  concatenate(a.host-name, " ", as(<string>, a.ipv4-address));
end;

define method \< (a :: <a-record>, b :: <a-record>)
 => (res :: <boolean>)
  a.host-name < b.host-name
end;

define web-class <mail-exchange> (<object>)
  data mx-name :: <string>;
  data priority :: <integer> = 23;
end;

define method print-object (mx :: <mail-exchange>, stream :: <stream>)
 => ();
  format(stream, "MX: %s\n", as(<string>, mx));
end method;

define method as (class == <string>, mx :: <mail-exchange>)
 => (res :: <string>)
  concatenate(mx.mx-name, ":", integer-to-string(mx.priority));
end;

define method \< (a :: <mail-exchange>, b :: <mail-exchange>)
 => (res :: <boolean>)
  a.mx-name < b.mx-name;
end;

define web-class <nameserver> (<object>)
  data ns-name :: <string>;
end;

define method print-object (ns :: <nameserver>, stream :: <stream>)
 => ();
  format(stream, "NS: %s\n", as(<string>, ns));
end method;

define method as (class == <string>, ns :: <nameserver>)
 => (res :: <string>)
  ns.ns-name;
end;

define method \< (a :: <nameserver>, b :: <nameserver>)
 => (res :: <boolean>)
  a.ns-name < b.ns-name;
end;

define web-class <zone> (<reference-object>)
  data zone-name :: <string>;
  data reverse? :: <boolean> = #f;
  has-many cname :: <cname>;
  data hostmaster :: <string> = "hostmaster.congress.ccc.de";
  data serial :: <integer> = 0;
  data refresh :: <integer> = 180;
  data retry :: <integer> = 300;
  data expire :: <integer> = 600;
  data time-to-live :: <integer> = 1800;
  data minimum :: <integer> = 300;
  has-many nameserver :: <nameserver>;
  has-many mail-exchange :: <mail-exchange>;
  has-many a-record :: <a-record>;
  //has-many text :: <string>;
end;

define method initialize (zone :: <zone>,
                          #rest rest, #key, #all-keys)
  next-method();
  for (ele in *nameserver*)
    zone.nameservers := add!(zone.nameservers, ele);
  end;
end;

define method print-object (zone :: <zone>, stream :: <stream>)
 => ();
  format(stream, "Zone: %s\n", as(<string>, zone));
end method;

define method as (class == <string>, zone :: <zone>)
 => (res :: <string>)
  zone.zone-name;
end;

define method \< (a :: <zone>, b :: <zone>) => (res :: <boolean>)
  if (b.reverse?)
    if (a.reverse?)
      a.zone-name < b.zone-name
    else
      #t
    end
  else
    if (a.reverse?)
      #f
    else
      a.zone-name < b.zone-name
    end
  end
end;

/*
define method print-bind-zone-file (print-zone :: <zone>, stream :: <stream>)
  format(stream, "@\tIN\tSOA\t%s.\t%s. (\n",
         print-zone.nameservers[0],
         print-zone.hostmaster);
  format(stream, "\t\t%d\t; Serial\n", print-zone.serial);
  format(stream, "\t\t%d\t; Refresh\n", print-zone.refresh);
  format(stream, "\t\t%d\t; Retry\n", print-zone.retry);
  format(stream, "\t\t%d\t; Expire\n", print-zone.expire);
  format(stream, "\t\t%d )\t; Minimum\n\n", print-zone.minimum);
  if (print-zone.reverse?)
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, print-zone.nameservers);
    do(method(x)
           format(stream, "%d\tIN\tPTR\n%s.%s.\n",
                  x.ipv4-address[3],
                  x.host-name,
                  print-zone.zone-name)
       end, choose(method(x)
                       ip-in-net?(parse-cidr(print-zone.zone-name),
                                  x.ipv4-address)
                   end, *config*.hosts));
  else
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, print-zone.nameservers);
    do(method(x)
           format(stream, "\tIN\tMX\t%d\t%s.\n", head(x), tail(x))
       end, print-zone.mail-exchanges);
    do(method(x)
           format(stream, "%s\tIN\tA\t%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address))
       end, choose(method(x)
                       x.zone = print-zone
                   end, *config*.hosts));
    do(method(x)
           format(stream, "%s\tCNAME\t%s\n", source(x), target(x))
       end, print-zone.cnames);
  end;
end;
*/

define method print-tinydns-zone-file (print-zone :: <zone>, stream :: <stream>)
  //Zfqdn:mname:rname:ser:ref:ret:exp:min:ttl:timestamp:lo
  format(stream, "Z%s:%s.:%s.\n", //:%d:%d:%d:%d:%d:%d\n",
         print-zone.zone-name, print-zone.nameservers[0].ns-name,
         print-zone.hostmaster); //, print-zone.serial,
//         print-zone.refresh, print-zone.retry,
//         print-zone.expire, print-zone.minimum,
//         print-zone.time-to-live);
  //nameserver
  do(method(x)
         format(stream, "&%s::%s.\n", print-zone.zone-name, x.ns-name)
     end, print-zone.nameservers);
  if (print-zone.reverse?)
    //PTR
    do(method(x)
           format(stream, "^%s.%s:%s:%d\n",
                  x.host-name,
                  x.zone.zone-name,
                  as(<string>, x.ipv4-address),
                  x.time-to-live);
       end, choose(method(y)
                       ip-in-net?(parse-cidr(print-zone.zone-name),
                                  y.ipv4-address)
                   end, *config*.hosts));
  else
    //MX
    do(method(x)
           format(stream, "@%s::%s.%s:%d\n",
                  print-zone.zone-name, mx-name(x), print-zone.zone-name, priority(x));
       end, print-zone.mail-exchanges);
    //Hosts
    do(method(x)
           format(stream, "+%s.%s:%s:%d\n",
                  x.host-name,
                  print-zone.zone-name,
                  as(<string>, x.ipv4-address),
                  x.time-to-live);
       end, choose(method(x)
                       x.zone = print-zone
                   end, *config*.hosts));
    //A
    do(method(x)
           format(stream, "+%s.%s:%s:%d\n",
                  x.host-name,
                  print-zone.zone-name,
                  as(<string>, x.ipv4-address),
                  x.time-to-live);
       end, print-zone.a-records);
    //CNAME
    do(method(x)
           format(stream, "C%s.%s:%s.%s\n",
                  source(x), print-zone.zone-name, target(x), print-zone.zone-name);
       end, print-zone.cnames);
  end;
end;

define method parse-cidr (zone-name :: <string>) => (network :: <network>)
  //zone-name is something like "1.2.3.in-addr.arpa." for the network 3.2.1.0/24
  let parts = split(zone-name, '.');
  let network-string
    = concatenate(parts[2], ".", parts[1], ".", parts[0], ".0");
  make(<network>, cidr: make(<cidr>,
                             network-address: make(<ip-address>, data: network-string),
                             netmask: 24));
end;

define method add-reverse-zones (network :: <network>) => ()
  //XXX: add hostmaster, mx, nameserver,...
  for (subnet in split-cidr(network.cidr, 24))
    let zone = make(<zone>,
                    reverse?: #t,
                    zone-name: cidr-to-reverse-zone(subnet),
                    visible?: #f);
    *config*.zones := add!(*config*.zones, zone);
  end;
end;
