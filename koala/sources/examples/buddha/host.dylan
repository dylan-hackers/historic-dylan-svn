module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <host> (<object>)
  data host-name :: <string>;
  data ipv4-address :: <ip-address>;
  data time-to-live :: <integer> = 300;
  data mac-address :: <mac-address>;
  has-a subnet;
  has-a zone;
end;

define method print-object (host :: <host>, stream :: <stream>)
 => ()
  format(stream, "Host: %s\n", as(<string>, host))
end;

define method \< (a :: <host>, b :: <host>) => (res :: <boolean>)
  a.ipv4-address < b.ipv4-address
end;

define method as (class == <string>, host :: <host>)
 => (res :: <string>)
  concatenate(host.host-name, " ", as(<string>, host.ipv4-address));
end;

define method print-isc-dhcpd-file (host :: <host>, stream :: <stream>)
 => ()
  format(stream, "host %s {\n", host.host-name);
  format(stream, "\thardware ethernet %s;\n", as(<string>, host.mac-address));
  format(stream, "\tfixed-address %s;\n", as(<string>, host.ipv4-address));
  format(stream, "}\n\n");
end;

