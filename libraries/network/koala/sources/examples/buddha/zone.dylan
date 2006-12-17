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
  data ipv4-address :: <ipv4-address>;
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

define web-class <aaaa-record> (<object>)
  data host-name :: <string>;
  data ipv6-address :: <ipv6-address>;
  data time-to-live :: <integer> = 300;
end;

define method as (class == <string>, a :: <aaaa-record>)
 => (res :: <string>)
  concatenate(a.host-name, " ", as(<string>, a.ipv6-address));
end;

define method \< (a :: <aaaa-record>, b :: <aaaa-record>)
 => (res :: <boolean>)
  a.host-name < b.host-name;
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
  slot used-names :: <string-table> = make(<string-table>);
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
  has-many aaaa-record :: <aaaa-record>;
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

define method print-tinydns-zone-file (print-zone :: <zone>,
                                       stream :: <stream>,
                                       #key reverse-table)
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
    if (reverse-table)
      let net = parse-cidr(print-zone.zone-name);
      let ip = net.cidr.cidr-network-address;
      while (ip < broadcast-address(net.cidr))
        let reverse-name = element(reverse-table,
                                   as(<string>, ip),
                                   default: concatenate("hacker-", get-ptr(ip)));
        format(stream, "^%d.%s:%s.%s:%d\n",
               ip[3],
               print-zone.zone-name,
               reverse-name,
               "congress.ccc.de",
               300);
        ip := ip + 1;
      end;
    end;
  else
    //MX
    do(method(x)
           format(stream, "@%s::%s.%s:%d\n",
                  print-zone.zone-name, mx-name(x), print-zone.zone-name, priority(x));
       end, print-zone.mail-exchanges);
    //Hosts
    do(method(x)
           if (reverse-table) reverse-table[as(<string>, x.ipv4-address)] := x.host-name end;
           format(stream, "+%s.%s:%s:%d\n",
                  x.host-name,
                  print-zone.zone-name,
                  as(<string>, x.ipv4-address),
                  x.time-to-live);
           unless (x.ipv6-address = $bottom-v6-address)
             format(stream, "3%s.%s:%s:%d\n",
                    x.host-name,
                    print-zone.zone-name,
                    as-dns-string(x.ipv6-address),
                    x.time-to-live);
           end;
       end, choose(method(x)
                       x.zone = print-zone
                   end, storage(<host>)));
    //A
    do(method(x)
           format(stream, "+%s.%s:%s:%d\n",
                  x.host-name,
                  print-zone.zone-name,
                  as(<string>, x.ipv4-address),
                  x.time-to-live);
       end, print-zone.a-records);
    //AAAA
    do(method(x)
         format(stream, "3%s.%s:%s:%d\n",
                x.host-name,
                print-zone.zone-name,
                as-dns-string(x.ipv6-address),
                x.time-to-live);
       end, print-zone.aaaa-records);
    //CNAME
    do(method(x)
           format(stream, "C%s.%s:%s.%s\n",
                  source(x), print-zone.zone-name, target(x), print-zone.zone-name);
       end, print-zone.cnames);
    //a records for dynamic PTR records
    let rev-net = storage(<network>)[0].cidr;
    let ip = rev-net.cidr-network-address;
    if (reverse-table)
      while (ip < broadcast-address(rev-net))
        unless (element(reverse-table, as(<string>, ip), default: #f))
          format(stream, "+%s.%s:%s:%d\n",
                 concatenate("hacker-", get-ptr(ip)),
                 print-zone.zone-name,
                 ip,
                 300);
        end;
        ip := ip + 1;
      end;
    end;
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
  let rev-mask = truncate/(network.cidr.cidr-netmask, 8) * 8;
  for (subnet in split-cidr(network.cidr, rev-mask))
    let zone = make(<zone>,
                    reverse?: #t,
                    zone-name: cidr-to-reverse-zone(subnet),
                    visible?: #f);
    block(ret)
      check(zone);
      let command = make(<add-command>,
                         arguments: list(zone, storage(<zone>)));
      let change = make(<change>,
                        command: command);
      save(change);
      redo(command);
      signal(make(<web-success>,
                  warning: concatenate("Added zone: ", show(zone))));
    exception (e :: <web-error>)
      signal(make(<web-form-warning>,
                  warning: concatenate("Couldn't add reverse zone, error was: ", e.error-string)));
      ret();
    end;
  end;
end;
