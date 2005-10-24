module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <cname> (<object>)
  slot source :: <string>, init-keyword: source:;
  slot target :: <string>, init-keyword: target:;
end;

define web-class <zone> (<object>)
  data zone-name :: <string>;
  data reverse? :: <boolean>;
  has-many cname;
  data hostmaster :: <string>;
  data serial :: <integer>;
  data refresh :: <integer>;
  data retry :: <integer>;
  data expire :: <integer>;
  data time-to-live :: <integer>;
  data minimum :: <integer>;
  has-many nameserver :: <host>;
  has-many mail-exchange :: <host>;
  has-many text :: <string>;
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
  a.zone-name < b.zone-name
end;

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

define method print-tinydns-zone-file (print-zone :: <zone>, stream :: <stream>)
  //Zfqdn:mname:rname:ser:ref:ret:exp:min:ttl:timestamp:lo
  format(stream, "Z%s:%s:%s:%d:%d:%d:%d:%d:%d\n",
         print-zone.zone-name, print-zone.nameservers[0],
         print-zone.hostmaster, print-zone.serial,
         print-zone.refresh, print-zone.retry,
         print-zone.expire, print-zone.minimum,
         print-zone.time-to-live);
  //nameserver
  do(method(x)
         format(stream, "&%s::%s\n", print-zone.zone-name, x)
     end, print-zone.nameservers);
  if (print-zone.reverse?)
    //PTR
    do(method(x)
           format(stream, "^%s:%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address));
       end, choose(method(x)
                       ip-in-net?(parse-cidr(print-zone.zone-name),
                                  x.ipv4-address)
                   end, *config*.hosts));
  else
    //MX
    do(method(x)
           format(stream, "@%s::%s:%d\n",
                  print-zone.zone-name, tail(x), head(x));
       end, print-zone.mail-exchanges);
    //A
    do(method(x)
           format(stream, "+%s:%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address));
       end, choose(method(x)
                       x.zone = print-zone
                   end, *config*.hosts));
    //CNAME
    do(method(x)
           format(stream, "C%s:%s\n",
                  source(x), target(x));
       end, print-zone.cnames);
  end;
end;

define method parse-cidr (zone-name :: <string>) => (network :: <network>)
  //XXX: needs to be done
  //zone-name should be something like 1.2.3.in-addr.arpa for 3.2.1.0/24
  make(<network>, cidr: "10.0.0.0/24");
end;

define method add-reverse-zones (network :: <network>) => ()
  //XXX: add reverse zone for each /24 in network
end;