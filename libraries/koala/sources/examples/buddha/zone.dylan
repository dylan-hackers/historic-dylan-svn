module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <cname> (<object>)
  slot source :: <string>, init-keyword: source:;
  slot target :: <string>, init-keyword: target:;
end;

define web-class <zone> (<object>)
  data zone-name :: <string>;
  data reverse? :: <boolean>;
  has-many host;
  has-many cname;
  data hostmaster :: <string>;
  data serial :: <integer>;
  data refresh :: <integer>;
  data retry :: <integer>;
  data expire :: <integer>;
  data time-to-live :: <integer>;
  data minimum :: <integer>;
  data nameserver :: <list>;
  data mail-exchange :: <list>;
  data text :: <list>;
end;

define method print-object (zone :: <zone>, stream :: <stream>)
 => ();
  format(stream, "%s", zone.zone-name);
end method;

define method as (class == <string>, zone :: <zone>)
 => (res :: <string>)
  zone.zone-name;
end;

define method gen-xml (zone :: <zone>)
  with-xml()
    tr { td(zone.zone-name) }
  end
end;

define method print-bind-zone-file (zone :: <zone>, stream :: <stream>)
  format(stream, "@\tIN\tSOA\t%s.\t%s. (\n",
         zone.nameserver[0],
         zone.hostmaster);
  format(stream, "\t\t%d\t; Serial\n", zone.serial);
  format(stream, "\t\t%d\t; Refresh\n", zone.refresh);
  format(stream, "\t\t%d\t; Retry\n", zone.retry);
  format(stream, "\t\t%d\t; Expire\n", zone.expire);
  format(stream, "\t\t%d )\t; Minimum\n\n", zone.minimum);
  if (zone.reverse?)
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, zone.nameserver);
    do(method(x)
           format(stream, "%d\tIN\tPTR\n%s.%s.\n",
                  x.ipv4-address[3],
                  x.host-name,
                  zone.zone-name)
       end, zone.hosts);
  else
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, zone.nameserver);
    do(method(x)
           format(stream, "\tIN\tMX\t%d\t%s.\n", head(x), tail(x))
       end, zone.mail-exchange);
    do(method(x)
           format(stream, "%s\tIN\tA\t%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address))
       end, zone.hosts);
    do(method(x)
           format(stream, "%s\tCNAME\t%s\n", source(x), target(x))
       end, zone.cnames);
  end;
end;

define method print-tinydns-zone-file (zone :: <zone>, stream :: <stream>)
  //Zfqdn:mname:rname:ser:ref:ret:exp:min:ttl:timestamp:lo
  format(stream, "Z%s:%s:%s:%d:%d:%d:%d:%d:%d\n",
         zone.zone-name, zone.nameserver[0],
         zone.hostmaster, zone.serial,
         zone.refresh, zone.retry,
         zone.expire, zone.minimum,
         zone.time-to-live);
  //nameserver
  do(method(x)
         format(stream, "&%s::%s\n", zone.zone-name, x)
     end, zone.nameserver);
  if (zone.reverse?)
    //PTR
    do(method(x)
           format(stream, "^%s:%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address));
       end, zone.hosts);
  else
    //MX
    do(method(x)
           format(stream, "@%s::%s:%d\n",
                  zone.zone-name, tail(x), head(x));
       end, zone.mail-exchange);
    //A
    do(method(x)
           format(stream, "+%s:%s\n",
                  x.host-name,
                  as(<string>, x.ipv4-address));
       end, zone.hosts);
    //CNAME
    do(method(x)
           format(stream, "C%s:%s\n",
                  source(x), target(x));
       end, zone.cnames);
  end;
end;
