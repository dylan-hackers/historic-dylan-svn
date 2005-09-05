module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <zone> (<object>)
  slot zone-name :: <string>, required-init-keyword: name:;
  slot zone-reverse? :: <boolean> = #f;
  slot zone-hosts :: <list> = #();
  slot zone-cnames :: <list> = #();
  slot zone-hostmaster :: <string>, init-keyword: hostmaster:;
  slot zone-serial :: <integer>, init-keyword: serial:;
  slot zone-refresh :: <integer>, init-keyword: refresh:;
  slot zone-retry :: <integer>, init-keyword: retry:;
  slot zone-expire :: <integer>, init-keyword: expire:;
  slot zone-time-to-live :: <integer>, init-keyword: time-to-live:;
  slot zone-minimum :: <integer>, init-keyword: minimum:;
  slot zone-nameserver :: <list>, init-keyword: nameserver:;
  slot zone-mail-exchange :: <list>, init-keyword: mail-exchange:;
  slot zone-text :: <list>, init-keyword: txt:;
end class;

define method print-object (zone :: <zone>, stream :: <stream>)
 => ();
  format(stream, "%s", zone.zone-name);
end method;

define method print-html (zone :: <zone>, stream :: <stream>)
 => ()
  gen-row(stream, list(zone.zone-name));
end;

define method gen-xml (zone :: <zone>)
  with-xml()
    tr { td(zone.zone-name) }
  end
end;

define method print-bind-zone-file (zone :: <zone>, stream :: <stream>)
  format(stream, "@\tIN\tSOA\t%s.\t%s. (\n",
         zone.zone-nameserver[0],
         zone.zone-hostmaster);
  format(stream, "\t\t%d\t; Serial\n", zone.zone-serial);
  format(stream, "\t\t%d\t; Refresh\n", zone.zone-refresh);
  format(stream, "\t\t%d\t; Retry\n", zone.zone-retry);
  format(stream, "\t\t%d\t; Expire\n", zone.zone-expire);
  format(stream, "\t\t%d )\t; Minimum\n\n", zone.zone-minimum);
  if (zone.zone-reverse?)
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, zone.zone-nameserver);
    do(method(x)
           format(stream, "%d\tIN\tPTR\n%s.%s.\n",
                  x.host-ipv4-address.ip[3],
                  x.host-name,
                  zone.zone-name)
       end, zone.zone-hosts);
  else
    do(method(x)
           format(stream, "\tIN\tNS\t%s. \n", x)
       end, zone.zone-nameserver);
    do(method(x)
           format(stream, "\tIN\tMX\t%d\t%s.\n", head(x), tail(x))
       end, zone.zone-mail-exchange);
    do(method(x)
           format(stream, "%s\tIN\tA\t%s\n",
                  x.host-name,
                  as(<string>, x.host-ipv4-address))
       end, zone.zone-hosts);
    do(method(x)
           format(stream, "%s\tCNAME\t%s\n", head(x), tail(x))
       end, zone.zone-cnames);
  end;
end;

define method print-tinydns-zone-file (zone :: <zone>, stream :: <stream>)
  //Zfqdn:mname:rname:ser:ref:ret:exp:min:ttl:timestamp:lo
  format(stream, "Z%s:%s:%s:%d:%d:%d:%d:%d:%d\n",
         zone.zone-name, zone.zone-nameserver[0],
         zone.zone-hostmaster, zone.zone-serial,
         zone.zone-refresh, zone.zone-retry,
         zone.zone-expire, zone.zone-minimum,
         zone.zone-time-to-live);
  //nameserver
  do(method(x)
         format(stream, "&%s::%s\n", zone.zone-name, x)
     end, zone.zone-nameserver);
  if (zone.zone-reverse?)
    //PTR
    do(method(x)
           format(stream, "^%s:%s\n",
                  x.host-name,
                  as(<string>, x.host-ipv4-address));
       end, zone.zone-hosts);
  else
    //MX
    do(method(x)
           format(stream, "@%s::%s:%d\n",
                  zone.zone-name, tail(x), head(x));
       end, zone.zone-mail-exchange);
    //A
    do(method(x)
           format(stream, "+%s:%s\n",
                  x.host-name,
                  as(<string>, x.host-ipv4-address));
       end, zone.zone-hosts);
    //CNAME
    do(method(x)
           format(stream, "C%s:%s\n",
                  head(x), tail(x));
       end, zone.zone-cnames);
  end;
end;
